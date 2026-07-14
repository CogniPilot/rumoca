use super::*;

impl SolveRuntime {
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

    pub fn record_visible_sample_if_new(
        &self,
        recorded_times: &mut Vec<f64>,
        data: &mut [Vec<f64>],
        solver_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(), RuntimeSolveError> {
        let mut values = self.visible_scratch.borrow_mut();
        self.visible_values_into(solver_y, params, t, &mut values)?;
        if recorded_times
            .last()
            .is_some_and(|last| sample_time_match_with_tol(*last, t))
        {
            if let Some(last) = recorded_times.last_mut() {
                *last = t;
            }
            replace_last_visible_values(data, &values)?;
            return Ok(());
        }
        reserve_runtime_vec_capacity(recorded_times, 1, "recorded sample times")?;
        recorded_times.push(t);
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

    fn visible_values_into(
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

    fn write_planned_visible_values(
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
            self.visible_value_rows
                .eval_single_output_rows_unchecked_with_context(
                    &plan.expression_rows,
                    y,
                    params,
                    t,
                    self.row_eval_context(),
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

    fn visible_values_for_names_from_rows(
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

    fn visible_value_from_row(
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
        self.overwrite_state_slots_preserving_algebraics(solver_y, state)
    }

    pub(super) fn overwrite_state_slots_preserving_algebraics(
        &self,
        solver_y: &mut [f64],
        state: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if solver_y.len() != self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "solver y has {} values, expected {}",
                solver_y.len(),
                self.solver_count
            )));
        }
        if state.len() < self.state_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "state has {} values, expected at least {}",
                state.len(),
                self.state_count
            )));
        }
        for (dst, src) in solver_y[..self.state_count]
            .iter_mut()
            .zip(state.iter().copied())
        {
            *dst = src;
        }
        Ok(())
    }

    // SPEC_0021: Exception - private derivative helper shares the public solver
    // callback shape while threading caller-owned scratch/output buffers.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn eval_state_derivatives_with_solver_y(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        solver_y: &mut Vec<f64>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.populate_solver_y_from_state(solver_y, state)?;
        self.refresh_derivative_dependencies(t, solver_y, params, tol, max_iters)?;
        // `eval_derivative_rhs_from_solver_y` fills `out` and *then* rejects
        // non-finite derivatives, so trace before propagating: on failure `out`
        // and `solver_y` still hold the offending values to name for the user.
        let eval_result = self.eval_derivative_rhs_from_solver_y(t, solver_y, params, out);
        crate::nan_trace::report_state_derivative(&self.model, t, solver_y, out);
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
        self.derivative_rhs
            .eval_with_context(solver_y, params, t, self.row_eval_context(), out)?;
        self.validate_finite_derivatives(out)
    }

    fn validate_finite_derivatives(&self, derivative: &[f64]) -> Result<(), RuntimeSolveError> {
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
