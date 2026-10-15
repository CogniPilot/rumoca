#[cfg(test)]
mod dispatch_tests;

use super::*;

pub(super) fn static_refresh_parameter_indices<'a>(
    implicit: ProgramAnalysis<'_>,
    plans: impl IntoIterator<Item = &'a solve::RefreshPlan>,
    program_rows: &FxHashMap<solve::RefreshScalarProgramSource, usize>,
) -> Box<[usize]> {
    let mut row_indices = BTreeSet::new();
    for plan in plans {
        row_indices.extend(
            plan.static_causal_rows()
                .iter()
                .filter_map(|row| program_rows.get(&row.source()).copied()),
        );
        for stage in &plan.value_stages {
            match stage {
                solve::RefreshStage::CausalSeedSweep { static_rows, .. }
                | solve::RefreshStage::ExactAssignments { static_rows, .. } => {
                    row_indices.extend(
                        plan.selected_rows(static_rows)
                            .iter()
                            .filter_map(|row| program_rows.get(&row.source()).copied()),
                    );
                }
                solve::RefreshStage::ProjectionBlock { .. } => {}
            }
        }
    }
    let mut parameters = BTreeSet::new();
    for row in row_indices {
        if let Some(indices) = implicit.row_parameter_indices(row) {
            parameters.extend(indices.iter().copied());
        }
    }
    parameters
        .into_iter()
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

impl ScalarExecutionView<'_> {
    pub(super) fn refresh_program_row(
        &self,
        row: &solve::AlgebraicRefreshRow,
    ) -> Result<usize, RuntimeSolveError> {
        self.scalar
            .refresh_program_rows
            .get(&row.source())
            .copied()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "construction-issued refresh program has no final scalar projection",
                )
            })
    }
    pub(super) fn refresh_derivative_dependencies(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.scalar.derivative_refresh,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )
    }

    pub fn refresh_algebraic_and_output_slots_certified(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.scalar.algebraic_refresh,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )
    }

    pub(crate) fn complete_algebraic_after_derivative(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        let plan = self
            .scalar
            .algebraic_refresh_after_derivative
            .as_ref()
            .map_or(&self.scalar.algebraic_refresh, |relation| {
                relation.remainder()
            });
        self.refresh_slots_with_plan(
            plan,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )
    }

    /// Refresh exactly the algebraic closure consumed by event iteration.
    ///
    /// The returned coordinate is certified for discrete assignments,
    /// conditions, relation memory, and event actions.  Unrelated visible
    /// outputs remain lazy until the caller requests the canonical full view.
    pub fn refresh_event_dependency_slots_certified(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        // Execute only construction-issued owners. Coincident clocks retain
        // their independent checked closures; later coverage certificates can
        // remove shared stages without runtime schedule discovery.
        self.refresh_slots_with_plan(
            &self.scalar.event_refresh,
            RefreshSlotArgs {
                t,
                solver_y: &mut *solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )?;
        for (clock_index, relation) in self
            .scalar
            .clock_event_refresh_after_event
            .iter()
            .enumerate()
        {
            let owner = self
                .model
                .problem
                .clocks
                .periodic_clock_id(clock_index)
                .ok_or_else(|| RuntimeSolveError::solve_ir("invalid event refresh clock"))?;
            if self.periodic_clock_active(owner, t, "event refresh")? {
                self.refresh_slots_with_plan(
                    relation.remainder(),
                    RefreshSlotArgs {
                        t,
                        solver_y: &mut *solver_y,
                        params,
                        tol,
                        max_iters,
                        certify_coordinates: true,
                    },
                )?;
            }
        }
        Ok(())
    }

    pub fn refresh_algebraic_and_output_slots(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.scalar.algebraic_refresh,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: false,
            },
        )
    }

    pub(super) fn refresh_slots_with_plan(
        &self,
        plan: &solve::RefreshPlan,
        mut args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        if plan.rows.is_empty() && plan.simultaneous_plan.is_empty() {
            return Ok(());
        }
        self.validate_refresh_inputs(args.solver_y, args.params)?;
        let mut incoming = self.scalar.refresh_snapshot_scratch.borrow_mut();
        copy_runtime_values_into(
            &mut incoming,
            args.solver_y,
            "algebraic projection snapshot",
        )?;
        // A dependency-complete causal schedule already proves the value
        // solution.  Executing the staged projection schedule as well would
        // replay every exact singleton after the complete causal seed sweep.
        // Apart from being redundant, that doubles the dominant continuous
        // callback work for fully explicit models.
        if plan.causal_solution_certified {
            let result = self.refresh_causal_seed_rows(plan, &mut args);
            if result.is_err() {
                args.solver_y.copy_from_slice(&incoming);
            }
            return result;
        }
        if self.value_stage_schedule_is_certified(plan) {
            let result = self.refresh_slots_with_stages(plan, &mut args, &incoming);
            if result.is_err() {
                args.solver_y.copy_from_slice(&incoming);
            }
            return result;
        }
        let mut causal_seed_failed = false;
        if !plan.causal_seed_rows.is_empty() {
            match self.refresh_causal_seed_rows(plan, &mut args) {
                Ok(()) => {}
                Err(error) => {
                    restore_after_causal_seed_error(error, args.solver_y, &incoming)?;
                    causal_seed_failed = true;
                }
            }
        }
        let result = self.project_refresh_slots(plan, &mut args, causal_seed_failed);
        if result.is_err() {
            args.solver_y.copy_from_slice(&incoming);
        }
        result
    }

    fn refresh_causal_seed_rows(
        &self,
        plan: &solve::RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_parameter_static_seed_rows(
            plan.static_causal_rows(),
            plan.static_causal_sequence,
            args.t,
            args.solver_y,
            args.params,
        )?;
        self.refresh_slots_once(
            plan.dynamic_causal_rows(),
            plan.dynamic_causal_sequence,
            args.t,
            args.solver_y,
            args.params,
        )
    }

    fn refresh_parameter_static_seed_rows(
        &self,
        rows: solve::RefreshRows<'_>,
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if rows.is_empty() {
            return Ok(());
        }
        self.prepare_static_refresh_cache(params, solver_y.len());
        self.refresh_prepared_static_rows(rows, sequence, t, solver_y, params)
    }

    pub(super) fn prepare_static_refresh_cache(&self, params: &[f64], solver_len: usize) {
        let mut cache = self.scalar.static_refresh_cache.borrow_mut();
        let params_match = cache.valid
            && cache.params.len() == self.scalar.static_refresh_parameter_indices.len()
            && cache
                .params
                .iter()
                .zip(self.scalar.static_refresh_parameter_indices.iter().copied())
                .all(|(lhs, index)| {
                    params
                        .get(index)
                        .is_some_and(|rhs| lhs.to_bits() == rhs.to_bits())
                });
        if !params_match {
            cache.valid = true;
            cache.params.clear();
            cache.params.extend(
                self.scalar
                    .static_refresh_parameter_indices
                    .iter()
                    .filter_map(|&index| params.get(index).copied()),
            );
            cache.values.clear();
            cache.values.resize(solver_len, None);
        }
    }

    pub(super) fn refresh_prepared_static_rows(
        &self,
        rows: solve::RefreshRows<'_>,
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        let fully_cached = {
            let cache = self.scalar.static_refresh_cache.borrow();
            rows.iter().all(|row| {
                cache
                    .values
                    .get(row.target_index())
                    .is_some_and(Option::is_some)
            })
        };
        if fully_cached {
            let cache = self.scalar.static_refresh_cache.borrow();
            for row in rows.iter() {
                solver_y[row.target_index()] =
                    cached_static_refresh_value(&cache, row.target_index())?;
            }
            return Ok(());
        }

        self.refresh_slots_once(rows, sequence, t, solver_y, params)?;
        let mut cache = self.scalar.static_refresh_cache.borrow_mut();
        for row in rows.iter() {
            cache.values[row.target_index()] = Some(solver_y[row.target_index()]);
        }
        Ok(())
    }

    pub(super) fn project_refresh_slots(
        &self,
        plan: &solve::RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        use_complete_plan: bool,
    ) -> Result<(), RuntimeSolveError> {
        let projection_plan = if use_complete_plan {
            &plan.simultaneous_plan
        } else {
            &plan.value_projection_plan
        };
        let projection_model = RefreshProjectionModel {
            runtime: *self,
            seed_linearizations: None,
            #[cfg(test)]
            plan: projection_plan,
            block_indices: &plan.simultaneous_block_indices,
            plan_validated: false,
            jacobian_v: ProjectionJacobian::SolverY(
                &self.scalar.implicit_projection_scalar_jacobian_v,
            ),
        };
        let projection_args = crate::runtime::projection::AlgebraicProjectionArgs {
            parameters: args.params,
            time: args.t,
            state_count: self.state_count,
            tolerance: args.tol,
        };
        if args.certify_coordinates {
            project_algebraics_with_plan_certified(
                &projection_model,
                projection_plan,
                args.solver_y,
                projection_args,
                args.max_iters,
            )
        } else {
            project_algebraics_with_plan(
                &projection_model,
                projection_plan,
                args.solver_y,
                projection_args,
                args.max_iters,
            )
        }
    }

    /// Project accepted state values onto lower-order constraints retained by
    /// structural index reduction.
    pub fn project_state_manifold(
        &self,
        solver_y: &mut [f64],
        params: &[f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let projection_model = RuntimeManifoldProjection { runtime: *self };
        crate::runtime::projection::project_state_manifold(
            &projection_model,
            solver_y,
            params,
            t,
            self.state_count,
            tol,
        )
    }

    /// Check an initialization point; this API cannot alter a state coordinate.
    pub fn certify_state_manifold(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tol: f64,
    ) -> Result<(), RuntimeSolveError> {
        crate::runtime::projection::certify_state_manifold(
            &RuntimeManifoldProjection { runtime: *self },
            y,
            p,
            t,
            tol,
        )
    }

    pub(super) fn validate_refresh_inputs(
        &self,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        // Refresh-plan construction already proves one implicit output for
        // every algebraic coordinate. Explicit states are owned by derivative
        // rows and therefore need no placeholder implicit rows.
        solve_eval::validate_input_requirements(
            self.scalar.implicit_scalar_rhs.source().requirements(),
            solver_y,
            params,
            None,
        )?;
        Ok(())
    }

    fn eval_refresh_row(
        &self,
        row: &solve::AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index();
        let value = self.eval_refresh_row_value(row, t, solver_y, params)?;
        // Catch non-finite results here (where the variable is known) and raise
        // a spanned diagnostic; otherwise a NaN slips through the iteration (the
        // `delta > max_delta` check is false for NaN) and only surfaces later as
        // an opaque "step size too small".
        if !value.is_finite() {
            return Err(self.non_finite_value_error(index, value));
        }
        Ok(value)
    }

    /// Solver slot name for diagnostics.
    pub(super) fn solver_name(&self, index: usize) -> String {
        self.model
            .problem
            .solver_name(index)
            .unwrap_or_else(|| "<unnamed>".to_owned())
    }

    /// Build a spanned non-finite-value error, resolving the solver slot's name
    /// and source span (from `variable_meta`) so the failure is traceable.
    pub(super) fn non_finite_value_error(&self, index: usize, value: f64) -> RuntimeSolveError {
        let name = self
            .model
            .problem
            .solver_name(index)
            .unwrap_or_else(|| format!("y[{index}]"));
        let span = self.solver_source_span(index);
        let kind = if value.is_nan() { "NaN" } else { "inf" };
        RuntimeSolveError::NonFiniteValue { name, kind, span }
    }

    pub(super) fn solver_source_span(&self, index: usize) -> Option<rumoca_core::Span> {
        let name = self.model.problem.solver_name(index)?;
        let point = self.model.visible_projection().index(&name)?;
        self.model
            .visible_projection()
            .metadata(point)
            .map(|meta| meta.source_span)
    }

    fn eval_refresh_row_value(
        &self,
        row: &solve::AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index();
        let program_row = self.refresh_program_row(row)?;
        // The assignment fast path is only valid when this plan entry updates
        // the row's own implicit target; for a cross-paired row (a coupled
        // block solved a residual row for one of its other unknowns) the
        // assignment value belongs to a different variable.
        if let Some(queries) = self.scalar.compiled_targets(false) {
            if row.assignment_target() == Some(index)
                && let Some(value) = queries.call(
                    (program_row, row.output_offset(), index),
                    (solver_y, params, t),
                    self.row_eval_context(),
                )?
            {
                return Ok(value);
            }
        } else if row.assignment_target() == Some(index)
            && let Some(reference) = self.scalar.implicit_scalar_rhs.reference()
            && let Some(value) = reference.eval_target_assignment_output_unchecked_with_context(
                rumoca_eval_solve::TargetAssignmentOutputRequest {
                    row_idx: program_row,
                    output_offset: row.output_offset(),
                    target_y_index: index,
                    y: solver_y,
                    p: params,
                    t,
                    context: self.row_eval_context(),
                },
            )?
        {
            return Ok(value);
        }
        let residual = self.refresh_row_residual(row, t, solver_y, params)?;
        self.solve_refresh_residual_row(row, residual, t, solver_y, params)
    }

    /// Evaluate one scalar view of the canonical implicit residual system.
    fn refresh_row_residual(
        &self,
        row: &solve::AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let program_row = self.refresh_program_row(row)?;
        self.scalar.implicit_scalar_rhs.eval_output(
            (program_row, row.output_offset()),
            RowEvalPoint {
                y: solver_y,
                p: params,
                t,
            },
            self.row_eval_context(),
        )
    }

    fn solve_refresh_residual_row(
        &self,
        row: &solve::AlgebraicRefreshRow,
        residual: f64,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index();
        let current = solver_y[index];
        let mut probe_y = self.scalar.refresh_probe_scratch.borrow_mut();
        probe_y.clear();
        reserve_runtime_vec_capacity(&mut probe_y, solver_y.len(), "refresh residual probe")?;
        probe_y.extend_from_slice(solver_y);
        probe_y[index] = current + 1.0;
        let probe_residual = self.refresh_row_residual(row, t, &probe_y, params)?;
        let slope = probe_residual - residual;
        if slope.is_finite() && slope.abs() > 1.0e-12 {
            return Ok(current - residual / slope);
        }
        // A residual that does not respond to the paired variable means the
        // refresh plan paired this row with a variable it cannot determine.
        // Nudging the value by the residual (the old fallback) converges to a
        // wrong but stable solution; fail loudly instead.
        Err(RuntimeSolveError::RefreshTargetUnassignable {
            row: row.equation_index(),
            target: self.solver_name(index).to_string(),
            span: self.solver_source_span(index),
        })
    }

    pub(super) fn refresh_slots_once(
        &self,
        plan: solve::RefreshRows<'_>,
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if self.try_native_assignment_refresh(sequence, t, solver_y, params)?
            || self.try_interpreted_assignment_refresh(sequence, t, solver_y, params)?
        {
            self.validate_refresh_values(plan, solver_y, params)?;
            return Ok(());
        }
        if let Some(reference) = self.scalar.implicit_scalar_rhs.reference()
            && self.can_batch_assignment_refresh(plan)
        {
            reference.apply_target_assignment_rows_unchecked_with_context(
                plan.iter(),
                |row| self.scalar.refresh_program_rows.get(&row.source()).copied(),
                solver_y,
                params,
                t,
                self.row_eval_context(),
            )?;
            self.validate_refresh_values(plan, solver_y, params)?;
            return Ok(());
        }
        let mut row_outputs = Vec::new();
        let mut row_pos = 0usize;
        while row_pos < plan.len() {
            if let Some(next_pos) =
                self.try_refresh_tensor_output_segment(plan, row_pos, t, solver_y, params)?
            {
                row_pos = next_pos;
                continue;
            }
            if let Some(next_pos) = self.try_refresh_shapeless_output_segment(
                plan,
                row_pos,
                t,
                solver_y,
                params,
                &mut row_outputs,
            )? {
                row_pos = next_pos;
                continue;
            }
            let refresh_row = &plan[row_pos];
            let index = refresh_row.target_index();
            let value = self.eval_refresh_row(refresh_row, t, solver_y, params)?;
            solver_y[index] = value;
            row_pos += 1;
        }
        Ok(())
    }

    fn try_native_assignment_refresh(
        &self,
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let Some(compiled) = self.compiled_assignment_schedule(sequence) else {
            return Ok(false);
        };
        compiled.call(solver_y, params, t, self.model.external_tables.as_slice())?;
        Ok(true)
    }

    fn compiled_assignment_schedule(
        &self,
        sequence: solve::RefreshSequenceId,
    ) -> Option<Rc<dyn CompiledSolveAssignmentSchedule>> {
        self.scalar
            .compiled_assignment_schedules
            .get(&sequence)
            .cloned()
    }

    fn validate_refresh_values(
        &self,
        plan: solve::RefreshRows<'_>,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        for row in plan.iter() {
            let value = solver_y[row.target_index()];
            if tracing::enabled!(target: "rumoca_solver::refresh_values", tracing::Level::TRACE) {
                self.trace_refresh_value(row, value, params);
            }
            if !value.is_finite() {
                return Err(self.non_finite_value_error(row.target_index(), value));
            }
        }
        Ok(())
    }

    fn trace_refresh_value(&self, row: &solve::AlgebraicRefreshRow, value: f64, params: &[f64]) {
        let source = row.source();
        let source_operations = usize::try_from(source.node())
            .ok()
            .and_then(|node| self.model.problem.continuous.implicit_rhs.nodes.get(node))
            .and_then(|node| match node {
                solve::ComputeNode::ScalarPrograms(programs) => usize::try_from(source.program())
                    .ok()
                    .and_then(|program| programs.program(program)),
                _ => None,
            });
        let parameter_dependencies = source_operations
            .and_then(|operations| {
                solve::StructuralPattern::derive_output_p_dependencies(operations, None).ok()
            })
            .and_then(|outputs| outputs.get(row.output_offset()).cloned())
            .map(|indices| {
                indices
                    .into_iter()
                    .filter_map(|index| params.get(index).copied().map(|value| (index, value)))
                    .collect::<Vec<_>>()
            });
        tracing::trace!(
            target: "rumoca_solver::refresh_values",
            target_index = row.target_index(),
            name = self.solver_name(row.target_index()),
            source_node = source.node(),
            source_program = source.program(),
            equation_index = row.equation_index(),
            output_offset = row.output_offset(),
            operations = ?source_operations,
            immutable_parameter_count = self.model.problem.solve_layout.parameter_count,
            parameter_dependencies = ?parameter_dependencies,
            value,
            "refresh assignment value"
        );
    }
}

impl ScalarExecutionView<'_> {
    pub fn eval_root_search_conditions_after_derivative_settle_into(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if !self.derivative_settled_coordinate_can_refresh_roots() {
            return Err(RuntimeSolveError::solve_ir(
                "root refresh has no certified derivative-settled remainder".to_string(),
            ));
        }
        let relation = self
            .scalar
            .root_refresh_after_derivative
            .as_ref()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir("root refresh derivative-settled remainder disappeared")
            })?;
        self.eval_root_search_conditions_after_settle(
            t, params, solver_y, tol, max_iters, out, relation,
        )
    }

    pub(crate) fn algebraic_settled_coordinate_can_refresh_roots(&self) -> bool {
        self.scalar.root_refresh_after_algebraic.is_some()
    }

    pub(crate) fn eval_root_search_conditions_after_algebraic_settle_into(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let relation = self
            .scalar
            .root_refresh_after_algebraic
            .as_ref()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "root refresh has no certified algebraic-settled remainder",
                )
            })?;
        self.eval_root_search_conditions_after_settle(
            t, params, solver_y, tol, max_iters, out, relation,
        )
    }

    // SPEC_0021: Exception - the selected constructor-issued remainder is part of the checked coordinate.
    #[allow(clippy::too_many_arguments)]
    fn eval_root_search_conditions_after_settle(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
        relation: &solve::RefreshRemainderRelation,
    ) -> Result<(), RuntimeSolveError> {
        if solver_y.len() != self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "settled solver-y length mismatch: expected {}, got {}",
                self.solver_count,
                solver_y.len()
            )));
        }
        let remainder = relation.remainder();
        if remainder.causal_solution_certified || !remainder.value_stages.is_empty() {
            self.refresh_slots_with_plan(
                remainder,
                RefreshSlotArgs {
                    t,
                    solver_y,
                    params,
                    tol,
                    max_iters,
                    certify_coordinates: true,
                },
            )?;
        }
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            match &self.scalar.root_condition_plan {
                Some(plan) if plan.search_rows.is_empty() => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_defaults(plan, params, t, model_out)?;
                }
                Some(plan) => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_conditions(
                        plan, solver_y, params, t, model_out,
                    )?;
                }
                None => self
                    .eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?,
            }
        }
        self.delay_runtime.evaluate_event_roots(
            t,
            solver_y,
            params,
            self.row_eval_context(),
            &mut out[model_root_count..],
        )?;
        validate_finite_runtime_output("root search output", out)
    }
}
