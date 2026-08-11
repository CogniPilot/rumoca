//! Atomic runtime boundary for checked tensor-native event transactions.

use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use crate::RuntimeSolveError;

use super::SolveRuntime;

/// Result of one atomic transaction invocation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EventTransactionExecution {
    pub changed: bool,
    pub failed_assertion: Option<usize>,
}

#[derive(Clone)]
pub(super) struct PreparedEventTransactionCoverage {
    pub(super) discrete_rows: Box<[bool]>,
    pub(super) guarded_assignments: Box<[bool]>,
    pub(super) structured_updates: Box<[bool]>,
    pub(super) event_actions: Box<[bool]>,
}

impl PreparedEventTransactionCoverage {
    pub(super) fn new(model: &solve::SolveModel) -> Self {
        let mut discrete_rows = vec![false; model.problem.discrete.rhs.len()];
        let mut guarded_assignments = vec![false; model.problem.discrete.guarded_assignments.len()];
        let mut structured_updates = vec![false; model.problem.discrete.structured_updates.len()];
        let mut event_actions = vec![false; model.problem.events.actions.len()];
        for transaction in &model.problem.discrete.event_transactions {
            mark_legacy_coverage(
                transaction,
                &mut discrete_rows,
                &mut guarded_assignments,
                &mut structured_updates,
            );
            mark_action_coverage(transaction, &mut event_actions);
        }
        Self {
            discrete_rows: discrete_rows.into_boxed_slice(),
            guarded_assignments: guarded_assignments.into_boxed_slice(),
            structured_updates: structured_updates.into_boxed_slice(),
            event_actions: event_actions.into_boxed_slice(),
        }
    }
}

fn mark_legacy_coverage(
    transaction: &solve::EventTransactionProgram,
    discrete_rows: &mut [bool],
    guarded_assignments: &mut [bool],
    structured_updates: &mut [bool],
) {
    for (target, owner) in transaction
        .targets()
        .iter()
        .zip(transaction.legacy_owners())
    {
        match *owner {
            solve::EventTransactionLegacyOwner::ScalarRows { start_row } => {
                let end = start_row + target.value_type().scalar_count() as usize;
                discrete_rows[start_row..end].fill(true);
            }
            solve::EventTransactionLegacyOwner::StructuredUpdate { update_index } => {
                structured_updates[update_index] = true;
            }
            solve::EventTransactionLegacyOwner::GuardedAssignment { program_index, .. } => {
                guarded_assignments[program_index] = true;
            }
        }
    }
}

fn mark_action_coverage(transaction: &solve::EventTransactionProgram, event_actions: &mut [bool]) {
    for indices in transaction.assertion_action_indices() {
        for &action_index in indices.iter() {
            event_actions[action_index] = true;
        }
    }
}

impl SolveRuntime {
    pub(super) fn evaluate_event_transactions_for_snapshot(
        &self,
        first_iteration: bool,
        observation_only: bool,
        skip_solver_or_time: bool,
        y: &[f64],
        p: &[f64],
        time: f64,
    ) -> Result<Vec<usize>, RuntimeSolveError> {
        if !first_iteration || observation_only {
            return Ok(Vec::new());
        }
        let mut evaluated = Vec::new();
        for transaction_index in 0..self.event_transaction_programs.len() {
            if !self.event_transaction_active_at(transaction_index, time)? {
                continue;
            }
            if skip_solver_or_time
                && self.event_transaction_reads_solver_or_time(transaction_index)?
            {
                continue;
            }
            self.eval_event_transaction_outputs(transaction_index, y, p, time)?;
            evaluated.push(transaction_index);
        }
        Ok(evaluated)
    }

    pub(super) fn commit_successful_event_transactions(
        &self,
        transaction_indices: impl IntoIterator<Item = usize>,
        y: &mut [f64],
        p: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let mut changed = false;
        for transaction_index in transaction_indices {
            if self
                .failed_event_transaction_assertion(transaction_index)?
                .is_none()
            {
                changed |= self.commit_event_transaction_targets(transaction_index, y, p)?;
            }
        }
        Ok(changed)
    }

    pub(super) fn project_event_transaction_action_values(
        &self,
        time: f64,
        values: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (transaction_index, transaction) in self
            .model
            .problem
            .discrete
            .event_transactions
            .iter()
            .enumerate()
        {
            if !self.event_transaction_active_at(transaction_index, time)? {
                continue;
            }
            self.project_one_event_transaction_action_values(
                transaction_index,
                transaction,
                values,
            )?;
        }
        Ok(())
    }

    fn project_one_event_transaction_action_values(
        &self,
        transaction_index: usize,
        transaction: &solve::EventTransactionProgram,
        values: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (assertion_index, action_indices) in
            transaction.assertion_action_indices().iter().enumerate()
        {
            let failed =
                self.event_transaction_assertion_failed(transaction_index, assertion_index)?;
            let value = if failed { 1.0 } else { 0.0 };
            for &action_index in action_indices.iter() {
                values[action_index] = value;
            }
        }
        Ok(())
    }

    pub(super) fn event_transaction_active_at(
        &self,
        transaction_index: usize,
        time: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let owner = self
            .model
            .problem
            .discrete
            .event_transactions
            .get(transaction_index)
            .ok_or_else(|| RuntimeSolveError::solve_ir("event transaction is out of bounds"))?;
        let Some(clock) = owner.clock_owner() else {
            return Ok(true);
        };
        let schedule = self
            .model
            .problem
            .clocks
            .periodic_schedule(clock)
            .ok_or_else(|| RuntimeSolveError::solve_ir("transaction clock is out of bounds"))?;
        Ok(crate::timeline::periodic_schedule_matches_time(
            schedule, time,
        ))
    }

    pub(super) fn event_transaction_reads_solver_or_time(
        &self,
        transaction_index: usize,
    ) -> Result<bool, RuntimeSolveError> {
        let program = self
            .model
            .problem
            .discrete
            .event_transactions
            .get(transaction_index)
            .ok_or_else(|| RuntimeSolveError::solve_ir("event transaction is out of bounds"))?;
        Ok(program.inputs().iter().any(|input| {
            matches!(
                input.source(),
                solve::ScalarSlot::Y { .. } | solve::ScalarSlot::Time
            )
        }))
    }

    /// Evaluate, check, and atomically commit one already-selected transaction.
    /// A failed predicate returns its action ordinal and commits no target.
    pub fn execute_event_transaction(
        &self,
        transaction_index: usize,
        y: &mut [f64],
        p: &mut [f64],
        time: f64,
    ) -> Result<EventTransactionExecution, RuntimeSolveError> {
        self.eval_event_transaction_outputs(transaction_index, y, p, time)?;
        let failed_assertion = self.failed_event_transaction_assertion(transaction_index)?;
        if failed_assertion.is_some() {
            return Ok(EventTransactionExecution {
                changed: false,
                failed_assertion,
            });
        }
        let changed = self.commit_event_transaction_targets(transaction_index, y, p)?;
        Ok(EventTransactionExecution {
            changed,
            failed_assertion: None,
        })
    }

    /// Evaluate one complete target/predicate tuple. Native and interpreter
    /// paths consume the same checked owner and aggregate input ordering.
    pub(super) fn eval_event_transaction_outputs(
        &self,
        transaction_index: usize,
        y: &[f64],
        p: &[f64],
        time: f64,
    ) -> Result<(), RuntimeSolveError> {
        let transaction = self.event_transaction(transaction_index)?;
        let mut input = self.event_transaction_input_scratch.borrow_mut();
        transaction.load_input_payload(y, p, time, &mut input)?;
        let mut outputs = self.event_transaction_output_scratch.borrow_mut();
        let output = outputs.get_mut(transaction_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("event transaction output scratch is out of bounds")
        })?;
        if let Some(compiled) = self
            .compiled_event_transactions
            .get(transaction_index)
            .and_then(Option::as_ref)
        {
            compiled.call(&input, output).map_err(|error| {
                RuntimeSolveError::solve_ir(format!(
                    "compiled event transaction {transaction_index} failed: {error}"
                ))
            })?;
            return Ok(());
        }
        transaction.eval_payload(&self.model.pure_calls, &input, output)?;
        Ok(())
    }

    /// Return the first failed checked assertion without committing any
    /// target. Predicate values are the scalar suffix proved by SOLVE-C55.
    pub(super) fn failed_event_transaction_assertion(
        &self,
        transaction_index: usize,
    ) -> Result<Option<usize>, RuntimeSolveError> {
        let transaction = self.event_transaction(transaction_index)?;
        let outputs = self.event_transaction_output_scratch.borrow();
        let output = outputs.get(transaction_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("event transaction output scratch is out of bounds")
        })?;
        if output.len() != transaction.output_scalar_count() {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction output scratch is not initialized",
            ));
        }
        Ok(output[transaction.target_scalar_count()..]
            .iter()
            .position(|predicate| *predicate == 0.0))
    }

    pub(super) fn event_transaction_assertion_failed(
        &self,
        transaction_index: usize,
        assertion_index: usize,
    ) -> Result<bool, RuntimeSolveError> {
        let transaction = self.event_transaction(transaction_index)?;
        if assertion_index >= transaction.program().assertion_count() {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction assertion index is out of bounds",
            ));
        }
        let outputs = self.event_transaction_output_scratch.borrow();
        let output = outputs.get(transaction_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("event transaction output scratch is out of bounds")
        })?;
        Ok(output[transaction.target_scalar_count() + assertion_index] == 0.0)
    }

    /// Commit the complete target tuple after every predicate has succeeded.
    /// All destination bounds are checked before the first write, making a
    /// partially committed transaction unrepresentable at this boundary.
    pub(super) fn commit_event_transaction_targets(
        &self,
        transaction_index: usize,
        y: &mut [f64],
        p: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let transaction = self.event_transaction(transaction_index)?;
        if self
            .failed_event_transaction_assertion(transaction_index)?
            .is_some()
        {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction targets cannot commit after a failed assertion",
            ));
        }
        let outputs = self.event_transaction_output_scratch.borrow();
        let output = outputs.get(transaction_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("event transaction output scratch is out of bounds")
        })?;
        let targets = transaction.scalar_targets();
        if targets.len() != transaction.target_scalar_count() {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction target adapter disagrees with its checked range count",
            ));
        }
        prevalidate_target_storage(targets, y, p)?;
        solve_eval::apply_scalar_slot_values_exact(
            targets,
            &output[..transaction.target_scalar_count()],
            y,
            p,
        )
        .map_err(Into::into)
    }

    fn event_transaction(
        &self,
        transaction_index: usize,
    ) -> Result<&solve_eval::PreparedEventTransactionProgram, RuntimeSolveError> {
        self.event_transaction_programs
            .get(transaction_index)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "event transaction {transaction_index} is out of bounds"
                ))
            })
    }
}

fn prevalidate_target_storage(
    targets: &[solve::ScalarSlot],
    y: &[f64],
    p: &[f64],
) -> Result<(), RuntimeSolveError> {
    for target in targets {
        let valid = match target {
            solve::ScalarSlot::Y { index, .. } => *index < y.len(),
            solve::ScalarSlot::P { index, .. } => *index < p.len(),
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => false,
        };
        if !valid {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction target is outside mutable runtime storage",
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroU64;

    use indexmap::IndexMap;
    use rumoca_core::{SourceId, Span};

    use super::*;

    fn transaction_model() -> solve::SolveModel {
        let span = Span::from_offsets(
            SourceId::from_source_name("event_transaction_runtime.mo"),
            0,
            1,
        );
        let provenance = span
            .require_provenance("event transaction runtime fixture")
            .unwrap();
        let integer_domain = solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap();
        let arithmetic = solve::SolveArithmeticProfile::construct(
            solve::SolveRealFormat::Binary64,
            solve::SolveRoundingMode::NearestTiesToEven,
            integer_domain,
        );
        let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
        let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
        let mut owner_id = None;
        let pure_calls = solve::SolvePureCallTable::construct(arithmetic, |table| {
            owner_id = Some(table.add_owner(
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
                vec![real.clone(), boolean.clone()],
                vec![
                    solve::SolvePureCallOutput::result(real.clone()),
                    solve::SolvePureCallOutput::assertion_predicate(),
                ],
                span,
                |builder, inputs, outputs| {
                    let value = builder.load(inputs[0], span)?;
                    let predicate = builder.load(inputs[1], span)?;
                    builder.store(outputs[0], value, span)?;
                    builder.store(outputs[1], predicate, span)
                },
            )?);
            Ok(())
        })
        .unwrap();
        let site = pure_calls
            .owner(owner_id.unwrap())
            .expect("owner resolves")
            .call_site();
        let transaction = solve::EventTransactionProgram::checked(
            solve::EventTransactionConstruction {
                site,
                inputs: vec![
                    (solve::scalar_slot_p(0), real.clone()),
                    (solve::scalar_slot_p(1), boolean),
                ],
                targets: vec![(solve::scalar_slot_p(2), real)],
                legacy_owners: vec![solve::EventTransactionLegacyOwner::ScalarRows {
                    start_row: 0,
                }],
                assertions: vec![solve::SolveEventAction {
                    kind: solve::SolveEventActionKind::Assert,
                    message: solve::SolveEventMessage {
                        parts: vec![solve::SolveEventMessagePart::Text("predicate".into())],
                    },
                    span,
                    origin: "fixture".into(),
                    clock_owner: None,
                }],
                assertion_action_indices: vec![vec![0]],
                statement_count: 1,
                clock_owner: None,
            },
            provenance,
        )
        .unwrap();
        let mut problem = solve::SolveProblem::default();
        problem.layout = solve::VarLayout::from_parts(IndexMap::new(), 0, 4);
        problem.solve_layout = solve::SolveLayout {
            variable_storage_runs: vec![solve::SolveVariableStorageRun {
                base: solve::scalar_slot_p(2),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::DiscreteReal,
                value_kind: solve::SolveVariableValueKind::Real,
            }],
            variable_declarations: vec![solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::DiscreteReal,
                solve::SolveVariableValueKind::Real,
            )],
            compiled_parameter_len: 4,
            discrete_real_scalar_names: vec!["target".into()],
            pre_param_bindings: vec![solve::PreParamBinding {
                dest_p_index: 3,
                source: solve::PreParamSource::P { index: 2 },
                clock_schedule: None,
            }],
            ..solve::SolveLayout::default()
        };
        problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            provenance,
        )
        .unwrap();
        problem.discrete.update_targets = vec![solve::scalar_slot_p(2)];
        problem.discrete.row_roles = vec![solve::DiscreteRowRole::Equation];
        problem.discrete.pre_modes = vec![solve::DiscreteEventPreMode::FollowCurrent];
        problem.discrete.observation_refresh = vec![false];
        problem.discrete.integrator_history_effects =
            vec![solve::IntegratorHistoryEffect::Preserve];
        problem.discrete.clock_owners = vec![None];
        problem.discrete.event_iteration_plan = solve::EventIterationPlan {
            runs: vec![solve::EventIterationRun {
                variable: 0,
                pre_binding_start: 0,
                owner: solve::EventIterationOwner::EventTransaction {
                    program_index: 0,
                    target_index: 0,
                },
            }],
        };
        problem.events.actions = transaction.assertions().to_vec();
        problem.events.action_conditions = solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            provenance,
        )
        .unwrap();
        problem.discrete.event_transactions.push(transaction);
        let model = solve::SolveModel {
            problem,
            pure_calls,
            parameters: vec![0.0; 4],
            ..solve::SolveModel::default()
        };
        model.validate().unwrap();
        model
    }

    #[test]
    fn failed_predicate_commits_no_target_and_success_commits_whole_tuple() {
        let model = transaction_model();
        let runtime = SolveRuntime::new(&model).unwrap();
        let mut y = Vec::new();
        let mut p = vec![4.5, 0.0, 9.0, 9.0];

        let failed = runtime
            .execute_event_transaction(0, &mut y, &mut p, 0.0)
            .unwrap();
        assert_eq!(failed.failed_assertion, Some(0));
        assert!(!failed.changed);
        assert_eq!(p[2], 9.0, "a failed tuple cannot partially commit");

        p[1] = 1.0;
        let committed = runtime
            .execute_event_transaction(0, &mut y, &mut p, 0.0)
            .unwrap();
        assert_eq!(committed.failed_assertion, None);
        assert!(committed.changed);
        assert_eq!(p[2], 4.5);
    }

    #[test]
    fn checked_transaction_replaces_legacy_row_and_executes_only_on_first_pass() {
        let model = transaction_model();
        let runtime = SolveRuntime::new(&model).unwrap();
        let mut y = Vec::new();
        let mut p = vec![7.25, 1.0, 9.0, 9.0];
        let first = super::super::event_update::DiscretePreSnapshot {
            row_filter: super::super::event_update::EventUpdateRowFilter::All,
            root_relation_overrides: &[],
            event_iteration: 0,
        };
        assert!(
            runtime
                .apply_constant_discrete_rows_for_pre_snapshot(&first, &mut y, &mut p, 0.0, 0.0,)
                .unwrap()
        );
        assert_eq!(
            p[2], 7.25,
            "the aggregate owner, not legacy Const(0), commits"
        );

        p[0] = 8.5;
        let later = super::super::event_update::DiscretePreSnapshot {
            event_iteration: 1,
            ..first
        };
        assert!(
            !runtime
                .apply_constant_discrete_rows_for_pre_snapshot(&later, &mut y, &mut p, 0.0, 0.0,)
                .unwrap()
        );
        assert_eq!(p[2], 7.25, "later event passes consume the held commit");
    }
}
