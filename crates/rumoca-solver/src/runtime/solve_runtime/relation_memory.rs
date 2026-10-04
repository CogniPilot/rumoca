use super::*;

impl SolveRuntime {
    pub fn update_relation_memory_from_state(
        &self,
        t: f64,
        state: &[f64],
        params: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        self.update_relation_memory_from_state_except_overrides(
            t,
            state,
            params,
            tol,
            max_iters,
            &[],
        )
    }

    pub(crate) fn update_relation_memory_from_state_except_overrides(
        &self,
        t: f64,
        state: &[f64],
        params: &mut [f64],
        tol: f64,
        max_iters: usize,
        root_relation_overrides: &[(usize, f64)],
    ) -> Result<bool, RuntimeSolveError> {
        if self
            .model
            .problem
            .events
            .root_relation_memory_targets
            .iter()
            .all(Option::is_none)
        {
            return Ok(false);
        }
        let roots = self.eval_root_conditions(t, state, params, tol, max_iters)?;
        self.update_root_relation_memory_from_values(&roots, params, root_relation_overrides)
    }

    /// Evaluate the dynamic-time deadline rows into a caller-owned buffer.
    pub fn eval_dynamic_time_event_rows_into(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let block = &self.model.problem.events.dynamic_time_event_rhs;
        validate_runtime_output_len("dynamic-time deadline output", block.len(), out.len())?;
        if block.is_empty() {
            return Ok(());
        }
        solve_eval::eval_scalar_program_block_with_context(
            block,
            solver_y,
            params,
            t,
            self.row_eval_context(),
            out,
        )?;
        Ok(())
    }

    pub fn current_dynamic_time_event_stop(
        &self,
        y: &[f64],
        params: &[f64],
        current_t: f64,
    ) -> Result<Option<RuntimeEventStop>, RuntimeSolveError> {
        current_dynamic_time_event_stop(&self.model, &self.runtime_state, y, params, current_t)
    }

    pub fn next_runtime_event_stop(
        &self,
        y: &[f64],
        params: &[f64],
        stop_schedule: &mut SolveStopSchedule,
        current_t: f64,
        target: f64,
    ) -> Result<(f64, Option<RuntimeEventStop>), RuntimeSolveError> {
        next_runtime_event_stop(
            &self.model,
            &self.runtime_state,
            y,
            params,
            stop_schedule,
            current_t,
            target,
        )
    }

    pub fn dynamic_time_event_stop_reads_solver_y(&self) -> bool {
        !self.model.problem.events.dynamic_time_event_rhs.is_empty()
    }

    pub fn apply_initialization_updates(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        solve_eval::eval_and_apply_update_rows(solve_eval::UpdateRowApplication {
            block: self.model.problem.initialization.update_rhs(),
            targets: self.model.problem.initialization.update_targets(),
            y,
            p,
            t,
            context: self.row_eval_context(),
            max_iters,
        })
        .map_err(Into::into)
    }

    pub fn apply_runtime_assignments_until_stable(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        self.apply_prepared_update_rows_until_stable(
            &self.runtime_assignment_rhs,
            &self.model.problem.discrete.runtime_assignment_targets,
            y,
            p,
            t,
            max_iters,
        )
    }

    pub fn apply_post_commit_assignments_until_stable(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        self.apply_prepared_update_rows_until_stable(
            &self.post_commit_assignment_rhs,
            &self.model.problem.discrete.post_commit_assignment_targets,
            y,
            p,
            t,
            max_iters,
        )
    }

    fn apply_prepared_update_rows_until_stable(
        &self,
        block: &solve_eval::PreparedScalarProgramBlock,
        targets: &[solve::ScalarSlot],
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        if block.is_empty() {
            return Ok(false);
        }
        if block.len() != targets.len() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "prepared update row count {} does not match target count {}",
                block.len(),
                targets.len()
            )));
        }
        let mut values = self.update_values_scratch.borrow_mut();
        values.resize(targets.len(), 0.0);
        let mut changed_any = false;
        for _ in 0..max_iters {
            block.eval_with_context(y, p, t, self.row_eval_context(), &mut values)?;
            let changed = solve_eval::apply_scalar_slot_values_exact(targets, &values, y, p)?;
            if !changed {
                return Ok(changed_any);
            }
            changed_any = true;
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "prepared update rows did not converge at t={t} after {max_iters} iterations"
        )))
    }

    pub fn apply_projected_event_update<P>(
        &self,
        input: ProjectedEventUpdateInput<'_>,
        mut project_algebraics: P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        self.validate_discrete_event_rows()?;
        let ProjectedEventUpdateInput {
            y,
            p,
            t,
            tol,
            event_pre_y,
            event_pre_p,
            max_iters,
            row_filter,
            root_relation_overrides,
        } = input;
        seed_event_entry_pre_params(&self.model, event_pre_y, event_pre_p, p)?;
        // Hidden mixed-condition clock lanes are compiler-owned projections of
        // their typed schedules. Materialize them in the canonical event P
        // view before any row evaluates; row-wide clock owners are only an
        // execution filter and cannot stand in for these expression leaves.
        write_clock_activation_params(&self.model, p, t);
        let window = self.event_schedule().relation_surface_window();
        let initial_event = self.initial_event_flag(p);
        // At the initialization event, pass 0 is the initialization instant
        // and its actions act on its values; the event iteration that
        // follows acts on the edges since that instant.
        let mut action_pre_p = std::borrow::Cow::Borrowed(event_pre_p);
        let mut history = std::collections::VecDeque::with_capacity(window + 1);
        for event_iteration in 0..self.event_schedule().fixed_point_cap() {
            // Appendix B fixes `pre` for one complete equation pass, then
            // advances ordinary event history atomically from that pass before
            // starting the next one.  Capture the source before any runtime
            // owner or algebraic projection can mutate the live view.
            let iter_pre_y = if event_iteration == 0 {
                event_pre_y.to_vec()
            } else {
                copy_runtime_values(y, "projected event iteration y snapshot")?
            };
            let iter_pre_p = if event_iteration == 0 {
                event_pre_p.to_vec()
            } else {
                copy_runtime_values(p, "projected event iteration p snapshot")?
            };
            let mut changed = if event_iteration == 0 {
                false
            } else {
                // MLS §8.6: `initial()` holds for the initialization pass
                // only; every later pass at this instant is the event
                // iteration that must follow it.
                self.set_initial_event_flag(p, false);
                advance_event_iteration_pre_params(
                    &self.model,
                    iter_pre_y.as_slice(),
                    iter_pre_p.as_slice(),
                    p,
                )?
            };
            changed |= self.run_event_pass(
                &mut DiscreteRowsSettleInput {
                    y,
                    p,
                    t,
                    tol,
                    max_iters,
                },
                row_filter,
                event_iteration,
                root_relation_overrides,
                &mut project_algebraics,
            )?;
            // Each activation buffer takes its condition on this pass's settled
            // values under this pass's `pre`, before the `pre` lanes advance.
            let snapshot = DiscretePreSnapshot {
                row_filter,
                root_relation_overrides,
                event_iteration,
            };
            changed |= self.advance_condition_memory(&snapshot, y, p, t)?;
            if !changed && event_iteration_plan_settled(&self.model, y, p)? {
                return self.converge_event(y, p, &action_pre_p, t, row_filter);
            }
            if initial_event
                && event_iteration == 0
                && let Some(outcome) =
                    self.close_initialization_instant(y, p, t, row_filter, &mut action_pre_p)?
            {
                return Ok(outcome);
            }
            if !on_relation_surface(&mut history, window, y, p, tol) {
                continue;
            }
            let resumed = self.resolve_relation_cycle(
                &history,
                &mut DiscreteRowsSettleInput {
                    y,
                    p,
                    t,
                    tol,
                    max_iters,
                },
                root_relation_overrides,
                &mut project_algebraics,
            )?;
            if resumed {
                history.clear();
            } else {
                return self.converge_event(y, p, &action_pre_p, t, row_filter);
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "event update iteration did not converge at t={t}"
        )))
    }

    /// The event iteration schedule this model's Solve IR carries
    /// (SPEC_0044 ME-EVENT-006).
    pub(super) fn event_schedule(&self) -> &solve::EventIterationSchedule {
        &self.model.problem.discrete.event_iteration_plan.schedule
    }

    /// Walk one event pass of the schedule under this pass's fixed `pre`.
    fn run_event_pass(
        &self,
        input: &mut DiscreteRowsSettleInput<'_>,
        row_filter: EventUpdateRowFilter,
        event_iteration: usize,
        root_relation_overrides: &mut Vec<(usize, f64)>,
        project_algebraics: &mut ProjectAlgebraics<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        let mut changed = false;
        for step in self.event_schedule().event_pass() {
            changed |= match step {
                solve::EventPassStep::RelationOverrides => self
                    .apply_root_relation_memory_overrides(
                        root_relation_overrides,
                        input.y,
                        input.p,
                        input.tol,
                    )?,
                solve::EventPassStep::RuntimeAssignments => self.run_runtime_assignments(input)?,
                solve::EventPassStep::AlgebraicProjection => project_algebraics(input.y, input.p)?,
                // Relation memory is an input to discrete equations and
                // algorithm transactions at this same event instant. Refresh
                // it from the just-projected coordinate before those consumers
                // take their one whole-event pass; updating it only afterward
                // lets fixed/clocked owners consume the stale side, and they
                // may not run again merely because relation memory changed.
                solve::EventPassStep::RelationRefresh => {
                    self.refresh_relations_then_assign(input, root_relation_overrides)?
                }
                solve::EventPassStep::RelationSettle => self
                    .settle_event_equations_with_fixed_pre(
                        input,
                        row_filter,
                        event_iteration,
                        root_relation_overrides,
                        project_algebraics,
                    )?,
            };
        }
        Ok(changed)
    }

    /// Refresh relation memory; when it changed, rerun the runtime
    /// assignments so discrete consumers read the refreshed side.
    fn refresh_relations_then_assign(
        &self,
        input: &mut DiscreteRowsSettleInput<'_>,
        root_relation_overrides: &mut Vec<(usize, f64)>,
    ) -> Result<bool, RuntimeSolveError> {
        let refreshed = self.refresh_event_relation_memory(
            input.t,
            input.y,
            input.p,
            input.tol,
            root_relation_overrides,
        )?;
        if refreshed {
            self.run_runtime_assignments(input)?;
        }
        Ok(refreshed)
    }

    fn run_runtime_assignments(
        &self,
        input: &mut DiscreteRowsSettleInput<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        self.apply_runtime_assignments_until_stable(
            input.y,
            input.p,
            input.t,
            input.tol,
            input.max_iters,
        )
    }

    fn settle_event_equations_with_fixed_pre(
        &self,
        input: &mut DiscreteRowsSettleInput<'_>,
        row_filter: EventUpdateRowFilter,
        event_iteration: usize,
        root_relation_overrides: &mut Vec<(usize, f64)>,
        project_algebraics: &mut ProjectAlgebraics<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        let mut changed_any = false;
        let window = self.event_schedule().relation_surface_window();
        let mut history = std::collections::VecDeque::with_capacity(window + 1);
        for relation_iteration in 0..self.event_schedule().fixed_point_cap() {
            let pass = self.run_relation_pass(
                input,
                row_filter,
                event_iteration.max(relation_iteration),
                root_relation_overrides,
                project_algebraics,
            )?;
            changed_any |= pass.changed;
            if pass.settled {
                return Ok(changed_any);
            }
            if !on_relation_surface(&mut history, window, input.y, input.p, input.tol) {
                continue;
            }
            // A cycle cannot leave on its own; a consistent mode of the
            // cycling relations, when one exists, is the event's answer.
            let resumed = self.resolve_relation_cycle(
                &history,
                input,
                root_relation_overrides,
                project_algebraics,
            )?;
            if !resumed {
                return Ok(changed_any);
            }
            history.clear();
            changed_any = true;
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "event condition equations did not converge with fixed pre at t={}",
            input.t
        )))
    }

    /// Search the modes of the relations a cycle alternates, together with
    /// every relation reading a coordinate one of them reads, for one whose
    /// projected coordinate gives each relation the side it was assigned
    /// (ME-EVENT-008). The first such mode is pinned through the relation
    /// overrides; the coordinate and parameters of a rejected mode are
    /// restored.
    fn search_consistent_mode(
        &self,
        history: &std::collections::VecDeque<(Vec<f64>, Vec<f64>)>,
        input: &mut DiscreteRowsSettleInput<'_>,
        root_relation_overrides: &mut Vec<(usize, f64)>,
        project_algebraics: &mut ProjectAlgebraics<'_>,
    ) -> Result<ModeSearch, RuntimeSolveError> {
        let limit = self.event_schedule().mode_search_relations();
        let candidates = self.mode_candidates(history, input.p)?;
        let mut roots = Vec::with_capacity(candidates.len());
        for (root, _) in &candidates {
            roots.push(*root);
        }
        if candidates.is_empty() || candidates.len() > limit {
            return Ok(ModeSearch::Unsearched { roots, limit });
        }
        let (base_y, base_p) = (input.y.to_vec(), input.p.to_vec());
        for mode in 0..(1usize << candidates.len()) {
            let sides = mode_sides(mode, candidates.len());
            if self.mode_is_consistent(&candidates, &sides, input, project_algebraics)? {
                drop_overrides_of(root_relation_overrides, &roots);
                root_relation_overrides.extend(roots.iter().copied().zip(sides));
                return Ok(ModeSearch::Found);
            }
            input.y.copy_from_slice(&base_y);
            input.p.copy_from_slice(&base_p);
        }
        Ok(ModeSearch::NoneConsistent { roots })
    }

    /// Resolve an iteration that entered a cycle (ME-EVENT-008): `true` when
    /// a consistent mode was pinned and the iteration continues; `false` when
    /// every joint mode was searched and none is consistent, so every state
    /// of the cycle is a fixed point and the current side is kept, counted as
    /// a relation-surface settle. A cycle the bounded search cannot cover is
    /// refused, naming its relations.
    fn resolve_relation_cycle(
        &self,
        history: &std::collections::VecDeque<(Vec<f64>, Vec<f64>)>,
        input: &mut DiscreteRowsSettleInput<'_>,
        root_relation_overrides: &mut Vec<(usize, f64)>,
        project_algebraics: &mut ProjectAlgebraics<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        let search = self.search_consistent_mode(
            history,
            input,
            root_relation_overrides,
            project_algebraics,
        )?;
        match search {
            ModeSearch::Found => Ok(true),
            ModeSearch::NoneConsistent { roots } => {
                crate::runtime::fallbacks::note_relation_surface(&roots);
                Ok(false)
            }
            ModeSearch::Unsearched { roots, limit } => Err(RuntimeSolveError::solve_ir(format!(
                "event iteration at t={} cycles among relations {roots:?}, which the \
                 mode search (limit {limit} relations) cannot cover",
                input.t
            ))),
        }
    }

    /// Roots whose relation memory differs across the cycle, and every root
    /// with relation memory that reads a coordinate one of them reads, with
    /// their memory parameters.
    fn mode_candidates(
        &self,
        history: &std::collections::VecDeque<(Vec<f64>, Vec<f64>)>,
        p: &[f64],
    ) -> Result<Vec<(usize, usize)>, RuntimeSolveError> {
        let events = &self.model.problem.events;
        let targets = events
            .root_relation_memory_targets
            .iter()
            .enumerate()
            .filter_map(|(root, target)| match target {
                Some(solve::ScalarSlot::P { index, .. }) => Some((root, *index)),
                _ => None,
            })
            .collect::<Vec<_>>();
        let cycling = targets
            .iter()
            .filter(|(_, index)| history.iter().any(|(_, prior)| prior[*index] != p[*index]))
            .map(|(root, _)| *root)
            .collect::<Vec<_>>();
        let neighborhoods = match solve::root_neighborhoods(&events.root_conditions) {
            Ok(neighborhoods) => neighborhoods,
            Err(error) => return Err(RuntimeSolveError::solve_ir(error.to_string())),
        };
        let joined = cycling
            .iter()
            .filter_map(|root| neighborhoods.get(*root))
            .flatten()
            .copied()
            .collect::<std::collections::BTreeSet<_>>();
        Ok(targets
            .into_iter()
            .filter(|(root, _)| joined.contains(root))
            .collect())
    }

    /// Project with `sides` assigned to `candidates` and report whether every
    /// candidate relation then takes its assigned side. A mode whose
    /// projection does not converge is not a solution.
    fn mode_is_consistent(
        &self,
        candidates: &[(usize, usize)],
        sides: &[f64],
        input: &mut DiscreteRowsSettleInput<'_>,
        project_algebraics: &mut ProjectAlgebraics<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        for ((_, index), side) in candidates.iter().zip(sides) {
            input.p[*index] = *side;
        }
        if project_algebraics(input.y, input.p).is_err() {
            return Ok(false);
        }
        let roots = self.eval_root_conditions_from_solver_y(input.t, input.y, input.p)?;
        let domains = &self.model.problem.events.root_zero_domains;
        Ok(candidates.iter().zip(sides).all(|((root, _), side)| {
            let value =
                crate::runtime::solve_ops::orient_typed_root_zero(roots[*root], domains[*root]);
            value == 0.0 || relation_memory_value_from_root(value) == *side
        }))
    }

    /// Walk one relation pass, stopping where its relations settled.
    fn run_relation_pass(
        &self,
        input: &mut DiscreteRowsSettleInput<'_>,
        row_filter: EventUpdateRowFilter,
        event_iteration: usize,
        root_relation_overrides: &mut Vec<(usize, f64)>,
        project_algebraics: &mut ProjectAlgebraics<'_>,
    ) -> Result<RelationPassOutcome, RuntimeSolveError> {
        let mut changed_any = false;
        for step in self.event_schedule().relation_pass() {
            let Some(changed) = self.run_relation_step(
                *step,
                input,
                row_filter,
                event_iteration,
                root_relation_overrides,
                project_algebraics,
            )?
            else {
                return Ok(RelationPassOutcome {
                    changed: changed_any,
                    settled: true,
                });
            };
            changed_any |= changed;
        }
        Ok(RelationPassOutcome {
            changed: changed_any,
            settled: false,
        })
    }

    /// Run one relation-pass step; `None` when the pass has settled.
    fn run_relation_step(
        &self,
        step: solve::RelationPassStep,
        input: &mut DiscreteRowsSettleInput<'_>,
        row_filter: EventUpdateRowFilter,
        event_iteration: usize,
        root_relation_overrides: &mut Vec<(usize, f64)>,
        project_algebraics: &mut ProjectAlgebraics<'_>,
    ) -> Result<Option<bool>, RuntimeSolveError> {
        let changed = match step {
            solve::RelationPassStep::DiscreteSettle => {
                let snapshot = DiscretePreSnapshot {
                    row_filter,
                    root_relation_overrides,
                    event_iteration,
                };
                self.settle_discrete_rows_for_pre_snapshot(&snapshot, input, project_algebraics)?
            }
            // MLS Appendix B: condition equations belong to the same solve as
            // current discrete values. Advancing pre before these relations
            // settle would preserve a transient latch value in event history.
            solve::RelationPassStep::SettledIfRelationsUnchanged => {
                let refreshed = self.refresh_event_relation_memory(
                    input.t,
                    input.y,
                    input.p,
                    input.tol,
                    root_relation_overrides,
                )?;
                if !refreshed {
                    return Ok(None);
                }
                true
            }
            solve::RelationPassStep::AlgebraicProjection => project_algebraics(input.y, input.p)?,
            solve::RelationPassStep::RuntimeAssignments => self.run_runtime_assignments(input)?,
        };
        Ok(Some(changed))
    }

    pub(super) fn validate_discrete_event_rows(&self) -> Result<(), RuntimeSolveError> {
        validate_discrete_event_rows(&self.model)
    }

    pub(super) fn override_relation_memory_row_values(
        &self,
        root_relation_overrides: &[(usize, f64)],
        row_values: &mut [(solve::ScalarSlot, f64)],
    ) {
        for (root_idx, value) in root_relation_overrides {
            let Some(Some(target)) = self
                .model
                .problem
                .events
                .root_relation_memory_targets
                .get(*root_idx)
                .copied()
            else {
                continue;
            };
            if let Some((_, row_value)) = row_values
                .iter_mut()
                .find(|(row_target, _)| *row_target == target)
            {
                *row_value = *value;
            }
        }
    }

    pub(crate) fn apply_root_relation_memory_overrides(
        &self,
        root_relation_overrides: &[(usize, f64)],
        y: &mut [f64],
        p: &mut [f64],
        _tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let mut changed = false;
        for (root_idx, value) in root_relation_overrides {
            let Some(Some(target)) = self
                .model
                .problem
                .events
                .root_relation_memory_targets
                .get(*root_idx)
                .copied()
            else {
                continue;
            };
            changed |= solve_eval::apply_scalar_slot_value_exact(target, *value, y, p)?;
        }
        Ok(changed)
    }

    pub(crate) fn refresh_event_relation_memory(
        &self,
        t: f64,
        y: &[f64],
        p: &mut [f64],
        _tol: f64,
        root_relation_overrides: &mut Vec<(usize, f64)>,
    ) -> Result<bool, RuntimeSolveError> {
        if self
            .model
            .problem
            .events
            .root_relation_memory_targets
            .iter()
            .all(Option::is_none)
        {
            return Ok(false);
        }
        let roots = self.eval_root_conditions_from_solver_y(t, y, p)?;
        let released =
            self.release_reversed_relation_overrides(&roots, root_relation_overrides, |_| true)?;
        let changed =
            self.update_root_relation_memory_from_values(&roots, p, root_relation_overrides)?;
        Ok(released || changed)
    }

    pub(crate) fn update_algebraic_relation_memory_from_solver_y_except_overrides(
        &self,
        t: f64,
        y: &[f64],
        p: &mut [f64],
        root_relation_overrides: &mut Vec<(usize, f64)>,
    ) -> Result<bool, RuntimeSolveError> {
        let roots = self.eval_root_conditions_from_solver_y(t, y, p)?;
        let algebraic = |root_index| {
            self.model
                .problem
                .events
                .root_relation_refresh_roles
                .get(root_index)
                .is_some_and(|role| *role == solve::RootRelationRefreshRole::AlgebraicDependent)
        };
        let released =
            self.release_reversed_relation_overrides(&roots, root_relation_overrides, algebraic)?;
        let changed = self.update_root_relation_memory_from_values_where(
            &roots,
            p,
            root_relation_overrides,
            algebraic,
        )?;
        Ok(released || changed)
    }

    /// A located side seeds event iteration; it cannot constrain a relation
    /// whose inputs subsequently move to the opposite side (MLS Appendix B).
    /// Keep the selection at an exact zero and carry released selections out
    /// to post-commit canonicalization so they cannot be reintroduced there.
    fn release_reversed_relation_overrides<F>(
        &self,
        roots: &[f64],
        overrides: &mut Vec<(usize, f64)>,
        mut include: F,
    ) -> Result<bool, RuntimeSolveError>
    where
        F: FnMut(usize) -> bool,
    {
        if let Some((index, _)) = overrides.iter().find(|(index, _)| *index >= roots.len()) {
            return Err(RuntimeSolveError::solve_ir(format!(
                "event relation override {index} is outside the root vector"
            )));
        }
        let before = overrides.len();
        overrides.retain(|(index, selected)| {
            !include(*index)
                || roots[*index] == 0.0
                || relation_memory_value_from_root(roots[*index]) == *selected
        });
        Ok(overrides.len() != before)
    }

    pub(super) fn update_root_relation_memory_from_values(
        &self,
        roots: &[f64],
        p: &mut [f64],
        root_relation_overrides: &[(usize, f64)],
    ) -> Result<bool, RuntimeSolveError> {
        self.update_root_relation_memory_from_values_where(
            roots,
            p,
            root_relation_overrides,
            |_| true,
        )
    }

    pub(super) fn update_root_relation_memory_from_values_where<F>(
        &self,
        roots: &[f64],
        p: &mut [f64],
        root_relation_overrides: &[(usize, f64)],
        mut include: F,
    ) -> Result<bool, RuntimeSolveError>
    where
        F: FnMut(usize) -> bool,
    {
        let mut changed = false;
        for (root_index, (root, target)) in roots
            .iter()
            .zip(&self.model.problem.events.root_relation_memory_targets)
            .enumerate()
        {
            if !include(root_index) {
                continue;
            }
            let Some(target) = *target else {
                continue;
            };
            let solve::ScalarSlot::P {
                index: parameter_index,
                ..
            } = target
            else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "root relation-memory target {root_index} is not a parameter slot"
                )));
            };
            if self.relation_memory_root_is_overridden(
                root_index,
                parameter_index,
                root_relation_overrides,
            ) {
                continue;
            }
            let slot = p.get_mut(parameter_index).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "root relation-memory parameter index {parameter_index} is out of bounds"
                ))
            })?;
            let root = crate::runtime::solve_ops::orient_typed_root_zero(
                *root,
                self.model.problem.events.root_zero_domains[root_index],
            );
            let value = relation_memory_value_from_root(root);
            let before = *slot;
            tracing::trace!(
                target: "rumoca_solver::relation_memory",
                root_index,
                parameter_index,
                root,
                before,
                value,
                "refresh root relation memory"
            );
            changed |= before != value;
            *slot = value;
        }
        Ok(changed)
    }

    pub(super) fn relation_memory_root_is_overridden(
        &self,
        root_index: usize,
        parameter_index: usize,
        root_relation_overrides: &[(usize, f64)],
    ) -> bool {
        root_relation_overrides.iter().any(|(override_index, _)| {
            if *override_index != root_index {
                return false;
            }
            matches!(
                self.model
                    .problem
                    .events
                    .root_relation_memory_targets
                    .get(root_index),
                Some(Some(solve::ScalarSlot::P { index, .. })) if *index == parameter_index
            )
        })
    }

    pub fn eval_root_conditions_from_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return Ok(Vec::new());
        }
        let model_root_count = self.model.problem.events.root_conditions.len();
        let mut values = zero_runtime_values(root_count, "root condition output")?;
        self.eval_root_conditions_from_refreshed_solver_y(
            t,
            y,
            p,
            &mut values[..model_root_count],
        )?;
        self.delay_runtime.evaluate_event_roots(
            t,
            y,
            p,
            self.row_eval_context(),
            &mut values[model_root_count..],
        )?;
        validate_finite_runtime_output("root condition output", &values)?;
        Ok(values)
    }

    /// MLS §8.6: `initial()` holds for the initialization pass only, so the
    /// actions of the initialization instant are evaluated on its settled
    /// values once, on the edges since event entry, and the event iteration
    /// that follows acts on the edges since this pass. Returns the outcome
    /// when an action ends the event.
    fn close_initialization_instant(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        row_filter: EventUpdateRowFilter,
        action_pre_p: &mut std::borrow::Cow<'_, [f64]>,
    ) -> Result<Option<EventActionOutcome>, RuntimeSolveError> {
        let outcome = self.eval_event_actions(y, p, action_pre_p, t, row_filter)?;
        if outcome != EventActionOutcome::Continue {
            return Ok(Some(outcome));
        }
        *action_pre_p =
            std::borrow::Cow::Owned(copy_runtime_values(p, "initialization action edges")?);
        Ok(None)
    }

    /// Close a converged event: evaluate its actions on the edges since
    /// `action_pre_p`, then release the clock pulses its condition buffers saw
    /// (see `release_condition_pulses`).
    fn converge_event(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        action_pre_p: &[f64],
        t: f64,
        row_filter: EventUpdateRowFilter,
    ) -> Result<EventActionOutcome, RuntimeSolveError> {
        let outcome = self.eval_event_actions(y, p, action_pre_p, t, row_filter)?;
        self.release_condition_pulses(y, p, t)?;
        Ok(outcome)
    }

    pub fn eval_event_actions(
        &self,
        y: &[f64],
        p: &[f64],
        event_pre_p: &[f64],
        t: f64,
        row_filter: EventUpdateRowFilter,
    ) -> Result<EventActionOutcome, RuntimeSolveError> {
        let events = &self.model.problem.events;
        let mut action_p = event_action_params(events, p, event_pre_p)?;
        // A clock ticks in the passes that run its rows: a pass settling only
        // unowned rows (the initialization instant before a phase-zero tick,
        // a coincident root's right limit) or a held right limit reads every
        // clock lane, owner and `when` leaf alike, as not ticking.
        let clocks_tick = row_filter.accepts(crate::EventPreMode::EventEntry, true);
        if clocks_tick {
            write_clock_activation_params(&self.model, &mut action_p, t);
        } else {
            crate::runtime::solve_ops::write_observation_clock_activation_params(
                &self.model,
                &mut action_p,
            );
        }
        let mut values = vec![0.0; events.actions.len()];
        let mut active_rows = self.event_action_active_row_indices.borrow_mut();
        active_rows.clear();
        for (row, action) in events.actions.iter().enumerate() {
            if self.event_transaction_coverage.event_actions[row] {
                continue;
            }
            let active = match action.clock_owner {
                Some(owner) => {
                    clocks_tick && self.periodic_clock_active(owner, t, "event action")?
                }
                None => true,
            };
            if active {
                active_rows.push(row);
            }
        }
        self.eval_selected_outputs_with_native(
            SpecializedRows {
                block: &self.event_action_conditions,
                cache: &self.compiled_event_action_rows,
                failed: &self.failed_event_action_rows,
            },
            &active_rows,
            RowEvalPoint { y, p: &action_p, t },
            &mut values,
        )?;
        self.project_event_transaction_action_values(t, row_filter, &mut values)?;
        self.report_violated_warnings(&values, y, &action_p, t)?;
        self.report_model_messages(&values, y, &action_p, t)?;
        match solve_eval::event_action_request_from_values(
            events,
            y,
            &action_p,
            t,
            self.row_eval_context(),
            values,
        )? {
            solve_eval::EventActionRequest::Continue => Ok(EventActionOutcome::Continue),
            solve_eval::EventActionRequest::AssertionFailed { message } => {
                Ok(EventActionOutcome::AssertionFailed { time: t, message })
            }
            solve_eval::EventActionRequest::Terminate { message } => {
                Ok(EventActionOutcome::Terminated { time: t, message })
            }
        }
    }

    pub fn record_visible_sample(
        &self,
        data: &mut [Vec<f64>],
        solver_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(), RuntimeSolveError> {
        let mut values = self.visible_scratch.borrow_mut();
        self.visible_values_into(solver_y, params, t, &mut values)?;
        push_visible_values(data, &values)
    }

    pub fn visible_values(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut values = Vec::new();
        self.visible_values_into(y, params, t, &mut values)?;
        Ok(values)
    }

    pub(super) fn visible_values_into(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        values: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        if let Some(plan) = &self.visible_value_plan {
            resize_runtime_values(values, plan.entries.len(), 0.0, "visible values")?;
            self.write_planned_visible_values(plan, y, params, t, values)?;
            return Ok(());
        }
        if self.visible_value_rows.len() == self.model.visible_names.len() {
            resize_runtime_values(values, self.visible_value_rows.len(), 0.0, "visible values")?;
            self.visible_value_rows.eval_with_context(
                y,
                params,
                t,
                self.row_eval_context(),
                values,
            )?;
            return Ok(());
        }
        let computed =
            visible_values_with_context(&self.model, y, params, t, self.row_eval_context())?;
        copy_runtime_values_into(values, &computed, "visible values")
    }

    pub(super) fn write_planned_visible_values(
        &self,
        plan: &VisibleValuePlan,
        y: &[f64],
        params: &[f64],
        t: f64,
        values: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (slot, entry) in values.iter_mut().zip(plan.entries.iter().copied()) {
            if let VisibleValuePlanEntry::Direct(source) = entry {
                *slot = direct_visible_value(source, y, params, t)?;
            }
        }
        if !plan.expression_rows.is_empty() {
            self.eval_single_output_rows_with_native(
                SpecializedRows {
                    block: &self.visible_value_rows,
                    cache: &self.compiled_visible_rows,
                    failed: &self.failed_visible_rows,
                },
                &plan.expression_rows,
                RowEvalPoint { y, p: params, t },
                values,
            )?;
            copy_grouped_expression_values(plan, values)?;
        }
        Ok(())
    }

    pub fn visible_values_for_names(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        names: &[String],
    ) -> Result<IndexMap<String, f64>, RuntimeSolveError> {
        if self.visible_value_rows.len() == self.model.visible_names.len() {
            return self.visible_values_for_names_from_rows(y, params, t, names);
        }
        let all_values = self.visible_values(y, params, t)?;
        let mut values = IndexMap::new();
        reserve_runtime_index_map_capacity(&mut values, names.len(), "visible name values")?;
        for name in names {
            let Some(idx) = self.visible_name_index.get(name).copied() else {
                continue;
            };
            let value = all_values.get(idx).copied().ok_or_else(|| {
                visible_value_index_error(name, idx, all_values.len(), "visible values")
            })?;
            values.insert(name.clone(), value);
        }
        Ok(values)
    }

    pub(super) fn visible_values_for_names_from_rows(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        names: &[String],
    ) -> Result<IndexMap<String, f64>, RuntimeSolveError> {
        let mut values = IndexMap::new();
        reserve_runtime_index_map_capacity(&mut values, names.len(), "visible row name values")?;
        for name in names {
            if let Some(value) = self.visible_value_from_row(name, y, params, t)? {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    pub(super) fn visible_value_from_row(
        &self,
        name: &str,
        y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(idx) = self.visible_name_index.get(name).copied() else {
            return Ok(None);
        };
        if idx >= self.visible_value_rows.len() {
            return Err(visible_value_index_error(
                name,
                idx,
                self.visible_value_rows.len(),
                "visible value rows",
            ));
        }
        let value = self.visible_value_rows.eval_row_with_context(
            idx,
            y,
            params,
            t,
            self.row_eval_context(),
        )?;
        Ok(Some(value))
    }

    pub(super) fn populate_solver_y_from_state(
        &self,
        solver_y: &mut Vec<f64>,
        state: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        copy_runtime_values_into(solver_y, &self.model.initial_y, "solver y initial values")?;
        resize_runtime_values(solver_y, self.solver_count, 0.0, "solver y")?;
        for (dst, src) in solver_y.iter_mut().zip(state.iter().copied()) {
            *dst = src;
        }
        Ok(())
    }

    pub(super) fn update_solver_y_guess_from_state(
        &self,
        solver_y: &mut [f64],
        state: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if solver_y.len() != self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "algebraic warm-start length mismatch: expected {}, got {}",
                self.solver_count,
                solver_y.len()
            )));
        }
        for (dst, src) in solver_y.iter_mut().zip(state.iter().copied()) {
            *dst = src;
        }
        Ok(())
    }

    pub(super) fn eval_state_derivatives_at_solver_y(
        &self,
        t: f64,
        params: &[f64],
        tol: f64,
        max_iters: usize,
        solver_y: &mut [f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_derivative_dependencies(t, solver_y, params, tol, max_iters)?;
        // `eval_derivative_rhs_from_solver_y` fills `out` and *then* rejects
        // non-finite derivatives, so trace before propagating: on failure `out`
        // and `solver_y` still hold the offending values to name for the user.
        let eval_result = self.eval_derivative_rhs_from_solver_y(t, solver_y, params, out);
        solve_eval::nan_trace::report_state_derivative(&self.model, t, solver_y, out);
        eval_result
    }

    pub(super) fn eval_derivative_rhs_from_solver_y(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        validate_derivative_output_len(out, self.state_count)?;
        if let Some(compiled) = self.compiled_derivative_rhs.as_ref()
            && compiled
                .call(
                    solver_y,
                    params,
                    t,
                    self.model.external_tables.as_slice(),
                    out,
                )
                .is_ok()
        {
            return self.validate_finite_derivatives(out);
        }
        self.derivative_rhs
            .eval_with_context(solver_y, params, t, self.row_eval_context(), out)?;
        self.validate_finite_derivatives(out)
    }

    pub(super) fn validate_finite_derivatives(
        &self,
        derivative: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        for (idx, value) in derivative.iter().enumerate() {
            if !value.is_finite() {
                let state_name = self
                    .model
                    .visible_names
                    .get(idx)
                    .cloned()
                    .unwrap_or_else(|| format!("state[{idx}]"));
                return Err(RuntimeSolveError::NonFiniteDerivative { state_name });
            }
        }
        Ok(())
    }
}

/// What one relation pass did: whether it changed the coordinate, and whether
/// it stopped because its relations settled.
struct RelationPassOutcome {
    changed: bool,
    settled: bool,
}

/// Whether an event or relation iteration has returned to a state of one of
/// its last `window` passes: its discrete values and relation memory repeat
/// exactly while the coordinate stays within the solve tolerance. Every state
/// of such a cycle is a fixed point for the coordinate: its alternating
/// relations sit on a surface where their expression is continuous, as a
/// relation inside `smooth` does (ME-EVENT-008), so the current side is kept.
/// A window of zero disables the rule.
fn on_relation_surface(
    history: &mut std::collections::VecDeque<(Vec<f64>, Vec<f64>)>,
    window: usize,
    y: &[f64],
    p: &[f64],
    tol: f64,
) -> bool {
    if window == 0 {
        return false;
    }
    let repeats = history.iter().any(|(prior_y, prior_p)| {
        prior_p
            .iter()
            .zip(p)
            .all(|(a, b)| a.to_bits() == b.to_bits())
            && !crate::runtime_values_changed(prior_y, y, tol)
    });
    history.push_back((y.to_vec(), p.to_vec()));
    if history.len() > window {
        history.pop_front();
    }
    repeats
}

/// What a mode search over a relation cycle found.
enum ModeSearch {
    /// A consistent joint mode, now pinned through the relation overrides.
    Found,
    /// Every joint mode of these roots was projected and none was consistent.
    NoneConsistent { roots: Vec<usize> },
    /// The cycle has no relation candidates, or more than `limit`, so it was
    /// not searched.
    Unsearched { roots: Vec<usize>, limit: usize },
}

/// Remove the pinned sides of every root in `roots`, keeping the others in order.
fn drop_overrides_of(overrides: &mut Vec<(usize, f64)>, roots: &[usize]) {
    let mut kept = Vec::with_capacity(overrides.len());
    for entry in overrides.drain(..) {
        if !roots.contains(&entry.0) {
            kept.push(entry);
        }
    }
    *overrides = kept;
}

/// The relation sides one joint mode assigns: bit `k` of `mode` picks the
/// upper side for candidate `k`.
fn mode_sides(mode: usize, count: usize) -> Vec<f64> {
    let mut sides = Vec::with_capacity(count);
    for bit in 0..count {
        sides.push(if mode >> bit & 1 == 1 { 1.0 } else { 0.0 });
    }
    sides
}

#[cfg(test)]
mod override_tests {
    use super::*;

    #[test]
    fn dropping_overrides_keeps_only_the_other_roots_in_order() {
        let mut overrides = vec![(3, 1.0), (1, 0.0), (5, 1.0), (1, 1.0)];
        drop_overrides_of(&mut overrides, &[1, 4]);
        assert_eq!(overrides, vec![(3, 1.0), (5, 1.0)]);
    }
}
