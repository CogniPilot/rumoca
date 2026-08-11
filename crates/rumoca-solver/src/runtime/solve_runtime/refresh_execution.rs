use super::*;

pub(super) fn static_refresh_parameter_indices<'a>(
    implicit: &PreparedScalarProgramBlock,
    plans: impl IntoIterator<Item = &'a RefreshPlan>,
    program_rows: &HashMap<solve::RefreshScalarProgramSource, usize>,
) -> Box<[usize]> {
    let mut row_indices = BTreeSet::new();
    for plan in plans {
        row_indices.extend(
            plan.static_causal_seed_rows
                .iter()
                .filter_map(|row| program_rows.get(&row.source()).copied()),
        );
        for stage in &plan.value_stages {
            match stage {
                RefreshStage::CausalSeedSweep { static_rows, .. }
                | RefreshStage::ExactAssignments { static_rows, .. } => {
                    row_indices.extend(
                        static_rows
                            .iter()
                            .filter_map(|row| program_rows.get(&row.source()).copied()),
                    );
                }
                RefreshStage::ProjectionBlock { .. } => {}
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

impl SolveRuntime {
    pub(super) fn refresh_program_row(
        &self,
        row: &AlgebraicRefreshRow,
    ) -> Result<usize, RuntimeSolveError> {
        self.refresh_program_rows
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
            &self.derivative_refresh,
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

    pub fn refresh_algebraic_and_output_slots_certified(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.algebraic_refresh,
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
            &self.event_refresh,
            RefreshSlotArgs {
                t,
                solver_y: &mut *solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )?;
        for (clock_index, relation) in self.clock_event_refresh_after_event.iter().enumerate() {
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
            &self.algebraic_refresh,
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
        plan: &RefreshPlan,
        mut args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        if plan.rows.is_empty() && plan.simultaneous_plan.is_empty() {
            return Ok(());
        }
        self.validate_refresh_inputs(args.solver_y, args.params)?;
        let mut incoming = self.refresh_snapshot_scratch.borrow_mut();
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
        plan: &RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_parameter_static_seed_rows(
            &plan.static_causal_seed_rows,
            plan.static_causal_sequence,
            args.t,
            args.solver_y,
            args.params,
        )?;
        self.refresh_slots_once(
            &plan.dynamic_causal_seed_rows,
            plan.dynamic_causal_sequence,
            args.t,
            args.solver_y,
            args.params,
        )
    }

    fn refresh_parameter_static_seed_rows(
        &self,
        rows: &[AlgebraicRefreshRow],
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
        let mut cache = self.static_refresh_cache.borrow_mut();
        let params_match = cache.valid
            && cache.params.len() == self.static_refresh_parameter_indices.len()
            && cache
                .params
                .iter()
                .zip(self.static_refresh_parameter_indices.iter().copied())
                .all(|(lhs, index)| {
                    params
                        .get(index)
                        .is_some_and(|rhs| lhs.to_bits() == rhs.to_bits())
                });
        if !params_match {
            cache.valid = true;
            cache.params.clear();
            cache.params.extend(
                self.static_refresh_parameter_indices
                    .iter()
                    .filter_map(|&index| params.get(index).copied()),
            );
            cache.values.clear();
            cache.values.resize(solver_len, None);
        }
    }

    pub(super) fn refresh_prepared_static_rows(
        &self,
        rows: &[AlgebraicRefreshRow],
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        let fully_cached = {
            let cache = self.static_refresh_cache.borrow();
            rows.iter().all(|row| {
                cache
                    .values
                    .get(row.target_index())
                    .is_some_and(Option::is_some)
            })
        };
        if fully_cached {
            let cache = self.static_refresh_cache.borrow();
            for row in rows {
                solver_y[row.target_index()] =
                    cached_static_refresh_value(&cache, row.target_index())?;
            }
            return Ok(());
        }

        self.refresh_slots_once(rows, sequence, t, solver_y, params)?;
        let mut cache = self.static_refresh_cache.borrow_mut();
        for row in rows {
            cache.values[row.target_index()] = Some(solver_y[row.target_index()]);
        }
        Ok(())
    }

    pub(super) fn project_refresh_slots(
        &self,
        plan: &RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        use_complete_plan: bool,
    ) -> Result<(), RuntimeSolveError> {
        let projection_plan = if use_complete_plan {
            &plan.simultaneous_plan
        } else {
            &plan.value_projection_plan
        };
        let projection_model = RefreshProjectionModel {
            runtime: self,
            plan: projection_plan,
            block_indices: &plan.simultaneous_block_indices,
            plan_validated: false,
            jacobian_v: ProjectionJacobian::SolverY {
                block: &self.implicit_projection_jacobian_v,
                scalar: &self.implicit_projection_scalar_jacobian_v,
            },
        };
        let projection_args = crate::AlgebraicProjectionArgs {
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
        let projection_model = RuntimeManifoldProjection { runtime: self };
        crate::project_state_manifold(
            &projection_model,
            solver_y,
            params,
            t,
            self.state_count,
            tol,
        )
    }

    /// Whether checked Solve IR retained any lower-order state constraints.
    ///
    /// An empty projection artifact is a construction-time certificate that
    /// projecting continuous states cannot change them. FMI hosts use this to
    /// avoid reconstructing observation algebraics merely to discover that
    /// there is no manifold system to evaluate.
    pub fn requires_state_manifold_projection(&self) -> bool {
        !self
            .model
            .problem
            .continuous
            .manifold_projection_plan
            .is_empty()
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
            self.implicit_scalar_rhs.requirements(),
            solver_y,
            params,
            None,
        )?;
        Ok(())
    }

    fn eval_refresh_row(
        &self,
        row: &AlgebraicRefreshRow,
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
    pub(super) fn solver_name(&self, index: usize) -> &str {
        self.model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)
            .map_or("<unnamed>", String::as_str)
    }

    /// Build a spanned non-finite-value error, resolving the solver slot's name
    /// and source span (from `variable_meta`) so the failure is traceable.
    pub(super) fn non_finite_value_error(&self, index: usize, value: f64) -> RuntimeSolveError {
        let name = self
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)
            .cloned()
            .unwrap_or_else(|| format!("y[{index}]"));
        let span = self.solver_source_span(index);
        let kind = if value.is_nan() { "NaN" } else { "inf" };
        RuntimeSolveError::NonFiniteValue { name, kind, span }
    }

    pub(super) fn solver_source_span(&self, index: usize) -> Option<rumoca_core::Span> {
        let name = self
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)?;
        self.model
            .variable_meta
            .iter()
            .find(|meta| &meta.name == name)
            .map(|meta| meta.source_span)
    }

    fn eval_refresh_row_value(
        &self,
        row: &AlgebraicRefreshRow,
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
        if row.assignment_target() == Some(index)
            && let Some(value) = self
                .implicit_scalar_rhs
                .eval_target_assignment_output_unchecked_with_context(
                    program_row,
                    row.output_offset(),
                    index,
                    solver_y,
                    params,
                    t,
                    self.row_eval_context(),
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
        row: &AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let program_row = self.refresh_program_row(row)?;
        self.implicit_scalar_rhs
            .eval_row_output_unchecked_with_context(
                program_row,
                row.output_offset(),
                solver_y,
                params,
                t,
                self.row_eval_context(),
            )
            .map_err(Into::into)
    }

    fn solve_refresh_residual_row(
        &self,
        row: &AlgebraicRefreshRow,
        residual: f64,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index();
        let current = solver_y[index];
        let mut probe_y = self.refresh_probe_scratch.borrow_mut();
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
        plan: &[AlgebraicRefreshRow],
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if self.try_native_assignment_refresh(sequence, t, solver_y, params)? {
            self.validate_refresh_values(plan, solver_y)?;
            return Ok(());
        }
        if self.can_batch_assignment_refresh(plan) {
            self.implicit_scalar_rhs
                .apply_target_assignment_rows_unchecked_with_context(
                    plan,
                    |row| self.refresh_program_rows.get(&row.source()).copied(),
                    solver_y,
                    params,
                    t,
                    self.row_eval_context(),
                )
                .map_err(RuntimeSolveError::from)?;
            self.validate_refresh_values(plan, solver_y)?;
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
        let Some(backend) = self.execution_backend.as_ref() else {
            return Ok(false);
        };
        let cached = self
            .compiled_assignment_schedules
            .borrow()
            .get(&sequence)
            .cloned();
        let compiled = match cached {
            Some(Some(compiled)) => compiled,
            Some(None) => return Ok(false),
            None => {
                let owners = &self.model.problem.continuous.refresh_owners;
                let Some(schedule) = owners.exact_assignment_schedule(sequence) else {
                    self.compiled_assignment_schedules
                        .borrow_mut()
                        .insert(sequence, None);
                    return Ok(false);
                };
                if std::env::var_os("RUMOCA_PROFILE_IR").is_some() {
                    let programs = schedule
                        .program_ids()
                        .iter()
                        .filter_map(|id| owners.exact_assignment_program(*id));
                    let target_names = programs
                        .clone()
                        .flat_map(|program| program.target_indices().iter().copied())
                        .take(32)
                        .map(|target| self.solver_name(target))
                        .collect::<Vec<_>>();
                    let target_count = programs
                        .clone()
                        .map(|program| program.target_indices().len())
                        .sum::<usize>();
                    eprintln!(
                        "rumoca-assignment-schedule rows={} targets={} names={target_names:?}",
                        schedule.program_ids().len(),
                        target_count,
                    );
                    for program in programs {
                        let Ok(block) = program
                            .final_scalar_program(&self.model.problem.continuous.implicit_rhs)
                        else {
                            continue;
                        };
                        let [operations] = block.programs() else {
                            continue;
                        };
                        if operations.len() < 100
                            && !operations.iter().any(|operation| {
                                matches!(
                                    operation,
                                    rumoca_ir_solve::LinearOp::FunctionFold { .. }
                                        | rumoca_ir_solve::LinearOp::StoreOutputFunctionFold { .. }
                                )
                            })
                        {
                            continue;
                        }
                        let names = program
                            .target_indices()
                            .iter()
                            .take(8)
                            .map(|target| self.solver_name(*target))
                            .collect::<Vec<_>>();
                        let mut kinds = std::collections::BTreeMap::new();
                        for operation in operations {
                            *kinds.entry(operation.kind_name()).or_insert(0usize) += 1;
                        }
                        eprintln!(
                            "rumoca-assignment-program source={}:{} ops={} outputs=1 names={names:?} kinds={kinds:?}",
                            program.source().node(),
                            program.source().program(),
                            operations.len(),
                        );
                    }
                }
                let compiled = match backend.compile_assignment_schedule(
                    &self.model.problem.continuous.implicit_rhs,
                    owners,
                    schedule,
                ) {
                    Ok(compiled) => Some(compiled),
                    Err(error) => {
                        tracing::debug!(
                            target: "rumoca_solver::native_execution",
                            programs = schedule.program_ids().len(),
                            targets = schedule
                                .program_ids()
                                .iter()
                                .filter_map(|id| owners.exact_assignment_program(*id))
                                .map(|program| program.target_indices().len())
                                .sum::<usize>(),
                            %error,
                            "failed to compile assignment schedule"
                        );
                        None
                    }
                };
                self.compiled_assignment_schedules
                    .borrow_mut()
                    .insert(sequence, compiled.clone());
                if let Some(compiled) = compiled {
                    compiled
                } else {
                    self.compiled_assignment_schedules
                        .borrow_mut()
                        .insert(sequence, None);
                    return Ok(false);
                }
            }
        };
        if compiled
            .call(solver_y, params, t, self.model.external_tables.as_slice())
            .is_ok()
        {
            return Ok(true);
        }
        self.compiled_assignment_schedules
            .borrow_mut()
            .insert(sequence, None);
        Ok(false)
    }

    fn validate_refresh_values(
        &self,
        plan: &[AlgebraicRefreshRow],
        solver_y: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        for row in plan {
            let value = solver_y[row.target_index()];
            if !value.is_finite() {
                return Err(self.non_finite_value_error(row.target_index(), value));
            }
        }
        Ok(())
    }
}
