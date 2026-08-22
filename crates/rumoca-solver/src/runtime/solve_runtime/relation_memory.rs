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

    pub fn eval_scalar_program_block(
        &self,
        block: &solve::ScalarProgramBlock,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut values = zero_runtime_values(block.len(), "scalar program block output")?;
        solve_eval::eval_scalar_program_block_with_context(
            block,
            y,
            p,
            t,
            self.row_eval_context(),
            &mut values,
        )?;
        Ok(values)
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
            block: &self.model.problem.initialization.update_rhs,
            targets: &self.model.problem.initialization.update_targets,
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
        for event_iteration in 0..max_iters {
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
                advance_event_iteration_pre_params(
                    &self.model,
                    iter_pre_y.as_slice(),
                    iter_pre_p.as_slice(),
                    p,
                )?
            };
            changed |=
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            // Relation memory is an input to discrete equations and algorithm
            // transactions at this same event instant.  Refresh it from the
            // just-projected coordinate before those consumers take their one
            // whole-event pass; updating it only afterward lets fixed/clocked
            // owners consume the stale side and they are not permitted to run
            // again merely because relation memory changed later in the pass.
            let relation_changed_before_discrete = self
                .update_relation_memory_from_solver_y_except_overrides(
                    t,
                    y,
                    p,
                    tol,
                    root_relation_overrides,
                )?;
            changed |= relation_changed_before_discrete;
            if relation_changed_before_discrete {
                changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            }
            let snapshot = DiscretePreSnapshot {
                row_filter,
                root_relation_overrides,
                event_iteration,
            };
            {
                let mut settle_input = DiscreteRowsSettleInput {
                    y,
                    p,
                    t,
                    tol,
                    max_iters,
                };
                let discrete_changed = self.settle_discrete_rows_for_pre_snapshot(
                    &snapshot,
                    &mut settle_input,
                    &mut project_algebraics,
                )?;
                changed |= discrete_changed;
            }
            let relation_changed = self.update_relation_memory_from_solver_y_except_overrides(
                t,
                y,
                p,
                tol,
                root_relation_overrides,
            )?;
            changed |= relation_changed;
            let overrides_changed =
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
            changed |= overrides_changed;
            // The discrete settle returns from a projected coordinate with
            // runtime assignments stable. Reproject only if relation-memory
            // writes changed an input after that certificate; an unconditional
            // second full projection doubled unchanged clock ticks.
            if relation_changed || overrides_changed {
                changed |= project_algebraics(y, p)?;
                changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            }
            if !changed && event_iteration_plan_settled(&self.model, y, p)? {
                return self.eval_event_actions(y, p, event_pre_p, t, row_filter);
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "event update iteration did not converge at t={t}"
        )))
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

    pub(crate) fn update_relation_memory_from_solver_y_except_overrides(
        &self,
        t: f64,
        y: &[f64],
        p: &mut [f64],
        _tol: f64,
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
        let roots = self.eval_root_conditions_from_solver_y(t, y, p)?;
        self.update_root_relation_memory_from_values(&roots, p, root_relation_overrides)
    }

    pub(crate) fn update_algebraic_relation_memory_from_solver_y_except_overrides(
        &self,
        t: f64,
        y: &[f64],
        p: &mut [f64],
        root_relation_overrides: &[(usize, f64)],
    ) -> Result<bool, RuntimeSolveError> {
        let roots = self.eval_root_conditions_from_solver_y(t, y, p)?;
        self.update_root_relation_memory_from_values_where(
            &roots,
            p,
            root_relation_overrides,
            |root_index| {
                self.model
                    .problem
                    .events
                    .root_relation_refresh_roles
                    .get(root_index)
                    .is_some_and(|role| *role == solve::RootRelationRefreshRole::AlgebraicDependent)
            },
        )
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
            let value = relation_memory_value_from_root(*root);
            let before = *slot;
            tracing::trace!(
                target: "rumoca_solver::relation_memory",
                root_index,
                parameter_index,
                root = *root,
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
        write_clock_activation_params(&self.model, &mut action_p, t);
        let mut values = vec![0.0; events.actions.len()];
        let mut active_rows = self.event_action_active_row_indices.borrow_mut();
        active_rows.clear();
        for (row, action) in events.actions.iter().enumerate() {
            if self.event_transaction_coverage.event_actions[row] {
                continue;
            }
            let active = match action.clock_owner {
                Some(owner) => self.periodic_clock_active(owner, t, "event action")?,
                None => true,
            };
            if active {
                active_rows.push(row);
            }
        }
        self.eval_selected_outputs_with_native(
            &self.event_action_conditions,
            &self.compiled_event_action_rows,
            &self.failed_event_action_rows,
            &active_rows,
            y,
            &action_p,
            t,
            &mut values,
        )?;
        self.project_event_transaction_action_values(t, row_filter, &mut values)?;
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
                &self.visible_value_rows,
                &self.compiled_visible_rows,
                &self.failed_visible_rows,
                &plan.expression_rows,
                y,
                params,
                t,
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
