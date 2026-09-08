use super::*;

impl SolveRuntime {
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
        for (clock_index, refresh) in self.clock_event_refresh_after_event.iter().enumerate() {
            let owner = self
                .model
                .problem()
                .clocks()
                .periodic_clock_id(clock_index)
                .ok_or_else(|| RuntimeSolveError::solve_ir("invalid event refresh clock"))?;
            if self.periodic_clock_active(owner, t, "event refresh")? {
                self.refresh_slots_with_plan(
                    refresh,
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
        prepared: &PreparedRefreshPlan,
        mut args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let plan = &prepared.plan;
        if plan.rows().is_empty() && plan.simultaneous_plan().is_empty() {
            return Ok(());
        }
        self.validate_refresh_inputs(args.solver_y, args.params)?;
        let mut incoming = self.refresh_snapshot_scratch.borrow_mut();
        copy_runtime_values_into(
            &mut incoming,
            args.solver_y,
            "algebraic projection snapshot",
        )?;
        let result = match &prepared.execution {
            PreparedRefreshExecution::CertifiedCausal => {
                self.refresh_causal_seed_rows(prepared, &mut args)
            }
            PreparedRefreshExecution::CertifiedStages(stages) => {
                self.refresh_slots_with_stages(prepared, stages, &mut args)
            }
            PreparedRefreshExecution::FullProjection => {
                self.project_refresh_slots(plan, &mut args, true)
            }
        };
        if result.is_err() {
            args.solver_y.copy_from_slice(&incoming);
        }
        result
    }

    fn refresh_causal_seed_rows(
        &self,
        plan: &PreparedRefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_parameter_static_seed_rows(
            plan.static_causal_rows(),
            plan.static_causal_sequence(),
            args.t,
            args.solver_y,
            args.params,
        )?;
        self.refresh_slots_once(
            plan.dynamic_causal_rows(),
            plan.dynamic_causal_sequence(),
            args.t,
            args.solver_y,
            args.params,
        )
    }

    fn refresh_parameter_static_seed_rows(
        &self,
        rows: PreparedRefreshRows<'_>,
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
        rows: PreparedRefreshRows<'_>,
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
            for row in rows.iter() {
                solver_y[row.target_index()] =
                    cached_static_refresh_value(&cache, row.target_index())?;
            }
            return Ok(());
        }

        self.refresh_slots_once(rows, sequence, t, solver_y, params)?;
        let mut cache = self.static_refresh_cache.borrow_mut();
        for row in rows.iter() {
            cache.values[row.target_index()] = Some(solver_y[row.target_index()]);
        }
        Ok(())
    }

    pub(super) fn project_refresh_slots(
        &self,
        plan: &solve::IssuedRefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        use_complete_plan: bool,
    ) -> Result<(), RuntimeSolveError> {
        let projection_plan = if use_complete_plan {
            plan.simultaneous_plan()
        } else {
            plan.value_projection_plan()
        };
        let projection_model = RefreshProjectionModel {
            runtime: self,
            plan: projection_plan,
            block_indices: plan.simultaneous_block_indices(),
            plan_validated: false,
            jacobian_v: ProjectionJacobian::SolverY {
                block: &self.implicit_projection_jacobian_v,
                scalar: &self.implicit_projection_scalar_jacobian_v,
            },
        };
        let projection_args = crate::runtime::projection::AlgebraicProjectionArgs {
            parameters: args.params,
            time: args.t,
            state_count: self.state_count(),
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
        crate::runtime::projection::project_state_manifold(
            &projection_model,
            solver_y,
            params,
            t,
            self.state_count(),
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
            .problem()
            .continuous()
            .manifold_projection_plan()
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
        selected_arm: &ExactAssignmentPermit,
        row: PreparedRefreshRow<'_>,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index();
        let value = self.eval_refresh_row_value(selected_arm, row, t, solver_y, params)?;
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
        self.model()
            .problem()
            .solve_layout()
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
            .problem()
            .solve_layout()
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
            .problem()
            .solve_layout()
            .solver_maps
            .names
            .get(index)?;
        self.model()
            .variable_meta()
            .into_iter()
            .find(|meta| &meta.name == name)
            .map(|meta| meta.source_span)
    }

    fn eval_refresh_row_value(
        &self,
        selected_arm: &ExactAssignmentPermit,
        row: PreparedRefreshRow<'_>,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index();
        let program_row = row.program_row();
        // The assignment fast path is only valid when this plan entry updates
        // the row's own implicit target; for a cross-paired row (a coupled
        // block solved a residual row for one of its other unknowns) the
        // assignment value belongs to a different variable.
        if row.assignment_target() == Some(index)
            && let Some(value) = self
                .implicit_scalar_rhs
                .eval_target_assignment_output_unchecked_with_context(
                    rumoca_eval_solve::TargetAssignmentOutputRequest {
                        row_idx: program_row,
                        output_offset: row.output_offset(),
                        target_y_index: index,
                        y: solver_y,
                        p: params,
                        t,
                        context: selected_arm.row_eval_context(self),
                    },
                )?
        {
            return Ok(value);
        }
        let residual = self.refresh_row_residual(selected_arm, row, t, solver_y, params)?;
        self.solve_refresh_residual_row(selected_arm, row, residual, t, solver_y, params)
    }

    /// Evaluate one scalar view of the canonical implicit residual system.
    fn refresh_row_residual(
        &self,
        selected_arm: &ExactAssignmentPermit,
        row: PreparedRefreshRow<'_>,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        self.implicit_scalar_rhs
            .eval_row_output_unchecked_with_context(
                row.program_row(),
                row.output_offset(),
                solver_y,
                params,
                t,
                selected_arm.row_eval_context(self),
            )
            .map_err(Into::into)
    }

    fn solve_refresh_residual_row(
        &self,
        selected_arm: &ExactAssignmentPermit,
        row: PreparedRefreshRow<'_>,
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
        let probe_residual = self.refresh_row_residual(selected_arm, row, t, &probe_y, params)?;
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
        plan: PreparedRefreshRows<'_>,
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if plan.is_empty() {
            return Ok(());
        }
        let arm = self
            .execution_plan
            .exact_assignments
            .get(&sequence)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "refresh sequence has no preparation-issued execution arm",
                )
            })?;
        match arm {
            ExecutionArm::Native(compiled) => {
                self.run_native_assignment_refresh(compiled, sequence, t, solver_y, params)?;
                self.validate_refresh_values(plan, solver_y, params)
            }
            ExecutionArm::Interpreter(selected_arm) => {
                self.run_interpreted_assignment_refresh(selected_arm, plan, t, solver_y, params)
            }
        }
    }

    fn run_interpreted_assignment_refresh(
        &self,
        selected_arm: &ExactAssignmentPermit,
        plan: PreparedRefreshRows<'_>,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if self.can_batch_assignment_refresh(plan) {
            self.implicit_scalar_rhs
                .apply_target_assignment_rows_unchecked_with_context(
                    plan.iter().map(|row| (row.row(), row.program_row())),
                    solver_y,
                    params,
                    t,
                    selected_arm.row_eval_context(self),
                )
                .map_err(RuntimeSolveError::from)?;
            self.validate_refresh_values(plan, solver_y, params)?;
            return Ok(());
        }
        let mut row_outputs = Vec::new();
        let mut row_pos = 0usize;
        while row_pos < plan.len() {
            if let Some(next_pos) = self.try_refresh_tensor_output_segment(
                selected_arm,
                plan,
                row_pos,
                t,
                solver_y,
                params,
            )? {
                row_pos = next_pos;
                continue;
            }
            if let Some(next_pos) = self.try_refresh_shapeless_output_segment(
                selected_arm,
                plan,
                row_pos,
                super::refresh_batch::RefreshSegmentEvaluation {
                    t,
                    solver_y: &mut *solver_y,
                    params,
                },
                &mut row_outputs,
            )? {
                row_pos = next_pos;
                continue;
            }
            let refresh_row = plan
                .get(row_pos)
                .expect("prepared refresh iteration stays inside its checked selection");
            let index = refresh_row.target_index();
            let value = self.eval_refresh_row(selected_arm, refresh_row, t, solver_y, params)?;
            solver_y[index] = value;
            row_pos += 1;
        }
        Ok(())
    }

    fn run_native_assignment_refresh(
        &self,
        compiled: &Rc<dyn CompiledSolveAssignmentSchedule>,
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut scratch = self.native_assignment_scratch.borrow_mut();
        copy_runtime_values_into(
            &mut scratch,
            solver_y,
            "native exact-assignment transaction",
        )?;
        compiled.call(&mut scratch, params, t).map_err(|reason| {
            RuntimeSolveError::native_call(
                NativeExecutionOwner::ExactAssignment { sequence },
                reason,
            )
        })?;
        if scratch.len() != solver_y.len() {
            return Err(RuntimeSolveError::solve_ir(
                "native exact-assignment transaction changed solver-vector length",
            ));
        }
        solver_y.copy_from_slice(&scratch);
        Ok(())
    }

    fn validate_refresh_values(
        &self,
        plan: PreparedRefreshRows<'_>,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        for row in plan.iter() {
            let value = solver_y[row.target_index()];
            if tracing::enabled!(target: "rumoca_solver::refresh_values", tracing::Level::TRACE) {
                self.trace_refresh_value(row.row(), value, params);
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
            .and_then(|node| {
                self.model()
                    .problem()
                    .continuous()
                    .implicit_rhs()
                    .nodes
                    .get(node)
            })
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
            immutable_parameter_count = self.model().problem().solve_layout().parameter_count,
            parameter_dependencies = ?parameter_dependencies,
            value,
            "refresh assignment value"
        );
    }
}
