use super::*;

impl SolveMeKernel {
    pub(crate) fn continuous_state_derivatives_into(
        &self,
        derivatives: &mut [f64],
    ) -> Result<(), MeError> {
        if derivatives.len() != self.state_count {
            return Err(contract(format!(
                "continuous-state derivative buffer has {} entries for {} states",
                derivatives.len(),
                self.state_count,
            )));
        }
        self.require_active_lifecycle("get_continuous_state_derivatives")?;
        let time = self.continuous_eval_time();
        if let Some(cached) = self.cached_derivative(time, &self.states) {
            derivatives.copy_from_slice(&cached);
            return Ok(());
        }
        let settle = self.numerics_settle();
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.with_callback_solver_y(|guess| {
                self.runtime
                    .eval_state_derivatives_with_guess_into(
                        time,
                        &self.states,
                        params,
                        guess,
                        settle.tol,
                        settle.max_iters,
                        derivatives,
                    )
                    .map_err(MeError::from)
            })
        })
        .map_err(|error| error.at_stage(MeStage::Integration))?
        .map_err(|error| error.at_stage(MeStage::Integration))?;
        self.cache_derivative(time, &self.states, derivatives);
        Ok(())
    }

    pub(crate) fn event_indicators_into(&self, indicators: &mut [f64]) -> Result<(), MeError> {
        let indicator_count = self.runtime.root_condition_count();
        if indicators.len() != indicator_count {
            return Err(contract(format!(
                "event-indicator buffer has {} entries for {} indicators",
                indicators.len(),
                indicator_count,
            )));
        }
        self.require_active_lifecycle("get_event_indicators")?;
        let time = self.continuous_eval_time();
        if let Some(cached) = self.cached_root_conditions(time, &self.states) {
            indicators.copy_from_slice(&cached);
            return Ok(());
        }
        self.with_delay_evaluation_params(time, &self.states, |params| match self.root_profile {
            MeRootProfile::Component => self
                .runtime
                .eval_root_conditions_into(
                    time,
                    &self.states,
                    params,
                    ALGEBRAIC_REFRESH_TOL,
                    UPDATE_MAX_ITERS,
                    indicators,
                )
                .map_err(MeError::from),
            MeRootProfile::DiffsolFrozen => self
                .runtime
                .eval_root_search_conditions_into(
                    time,
                    &self.states,
                    params,
                    self.tolerance.max(1.0e-10),
                    256,
                    indicators,
                )
                .map_err(MeError::from),
        })
        .map_err(|error| error.at_stage(MeStage::Integration))?
        .map_err(|error| error.at_stage(MeStage::Integration))?;
        crate::orient_typed_root_zeros(
            indicators,
            &self.runtime.model.problem.events.root_zero_domains,
        );
        self.cache_root_conditions(time, &self.states, indicators);
        Ok(())
    }

    #[cfg(any(test, kani))]
    pub(crate) fn verification_observable_state(&self) -> (MeState, u64, Vec<u64>, Vec<u64>) {
        (
            self.lifecycle.state(),
            self.time.to_bits(),
            self.states.iter().map(|value| value.to_bits()).collect(),
            self.params.iter().map(|value| value.to_bits()).collect(),
        )
    }

    #[cfg(test)]
    pub(crate) fn verification_frozen_root_override_count(&self) -> usize {
        self.frozen_event_root_crossings.len()
    }

    #[cfg(test)]
    pub(crate) fn verification_canonicalize_committed_event_view(
        &mut self,
        event_time: f64,
        solver_y: &mut [f64],
    ) -> Result<(), MeError> {
        self.canonicalize_committed_event_view(event_time, solver_y, &[])
    }

    #[cfg(any(test, kani))]
    pub(crate) fn verification_matches_snapshot(&self, saved: &MeFmuState) -> bool {
        if !Rc::ptr_eq(&saved.instance_brand, &self.instance_brand) {
            return false;
        }
        let state = &saved.component;
        self.lifecycle.state() == state.lifecycle
            && self.stop_time.to_bits() == state.stop_time.to_bits()
            && self.time.to_bits() == state.time.to_bits()
            && option_float_bit_eq(self.event_boundary, state.event_boundary)
            && option_float_bit_eq(self.post_event_eval_time, state.post_event_eval_time)
            && self.event_anchor_time.to_bits() == state.event_anchor_time.to_bits()
            && float_slice_bit_eq(&self.states, &state.states)
            && float_slice_bit_eq(&self.params, &state.params)
            && self.stop_schedule.bit_eq(&state.stop_schedule)
            && option_event_entry_bit_eq(self.pending_event_entry, state.pending_event_entry)
            && option_event_entry_bit_eq(self.last_event_entry, state.last_event_entry)
            && option_event_stop_bit_eq(self.pending_event_stop, state.pending_event_stop)
            && self.advance_state_to_event_right_limit == state.advance_state_to_event_right_limit
            && self.state_time_coincidence == state.state_time_coincidence
            && self.initial_event_pending == state.initial_event_pending
            && self.skip_next_enter_continuous_delay_commit
                == state.skip_next_enter_continuous_delay_commit
            && root_crossings_bit_eq(&self.pending_root_crossings, &state.pending_root_crossings)
            && root_crossings_bit_eq(
                &self.frozen_event_root_crossings,
                &state.frozen_event_root_crossings,
            )
            && option_float_vec_bit_eq(&self.pending_event_pre_y, &state.pending_event_pre_y)
            && option_float_vec_bit_eq(&self.pending_event_pre_p, &state.pending_event_pre_p)
            && option_float_vec_bit_eq(&self.boundary_event_pre_y, &state.boundary_event_pre_y)
            && option_float_vec_bit_eq(&self.boundary_event_pre_p, &state.boundary_event_pre_p)
            && option_float_vec_bit_eq(
                &self.frozen_event_accepted_seed,
                &state.frozen_event_accepted_seed,
            )
            && float_slice_bit_eq(&self.solver_y_guess.borrow(), &state.solver_y_guess)
            && float_slice_bit_eq(
                &self.delay_params_scratch.borrow(),
                &state.delay_params_scratch,
            )
            && float_slice_bit_eq(
                &self.delay_solver_y_scratch.borrow(),
                &state.delay_solver_y_scratch,
            )
            && derivative_cache_bit_eq(
                self.derivative_cache.borrow().as_ref(),
                state.derivative_cache.as_ref(),
            )
            && root_cache_bit_eq(self.root_cache.borrow().as_ref(), state.root_cache.as_ref())
            && observations_bit_eq(&self.initial_observations, &state.initial_observations)
            && option_float_bit_eq(self.delay_step_limit, state.delay_step_limit)
            && self.last_projection_changed == state.last_projection_changed
            && termination_bit_eq(self.termination.as_ref(), state.termination.as_ref())
            && option_float_vec_bit_eq(
                &self.settled_initialization_y,
                &state.settled_initialization_y,
            )
            && self.runtime.matches_snapshot(&state.runtime)
    }

    pub(super) fn require_lifecycle_transition(
        &self,
        command: MeLifecycleCommand,
    ) -> Result<(), MeError> {
        self.lifecycle
            .next(command)
            .map(|_| ())
            .map_err(lifecycle_contract)
    }

    pub(super) fn commit_lifecycle_transition(
        &mut self,
        command: MeLifecycleCommand,
    ) -> Result<(), MeError> {
        self.lifecycle
            .transition(command)
            .map_err(lifecycle_contract)
    }

    pub(super) fn require_active_lifecycle(&self, operation: &'static str) -> Result<(), MeError> {
        if self.lifecycle.is_terminated() {
            return Err(contract(format!(
                "{operation} called after the component was terminated"
            )));
        }
        Ok(())
    }

    pub(super) fn require_observation_brand(
        &self,
        observation: &MeObservation,
    ) -> Result<(), MeError> {
        if !Rc::ptr_eq(&observation.instance_brand, &self.instance_brand) {
            return Err(contract("observation belongs to a different ME instance"));
        }
        Ok(())
    }

    /// `fmi3InstantiateModelExchange`: project the checked kernel once.
    ///
    /// Rejects a model the component cannot represent before any evaluation,
    /// per SPEC_0038 "Unsupported lifecycle capability fails before execution".
    pub fn instantiate(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
    ) -> Result<Self, MeError> {
        // `NoContinuousStates` is a routing answer, not a failure: a host reads
        // it to pick its zero-state path, so it stays unannotated.
        Self::instantiate_inner(source, config).map_err(|error| match error {
            routing @ MeError::NoContinuousStates => routing,
            failure => failure.at_stage(MeStage::Instantiate),
        })
    }

    /// Temporary phase-2 dual-run guard for the frozen Diffsol host.
    ///
    /// The frozen Diffsol driver still owns a full Solve vector during this
    /// migration step. Compare it inside the component so the adapter does not
    /// gain access to component-private algebraic storage. Delete this
    /// operation with [`MeNumericsProfile::DiffsolFrozen`].
    pub fn verify_frozen_compatibility_state(
        &self,
        expected_solver_y: &[f64],
        expected_parameters: &[f64],
        stage: MeStage,
    ) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Err(contract(
                "frozen compatibility state verification requires DiffsolFrozen numerics",
            )
            .at_stage(stage));
        }
        let actual_solver_y = self.solver_y_guess.borrow().clone();
        let state_slots: Vec<_> = (0..self.state_count).collect();
        if actual_solver_y.len() == expected_solver_y.len()
            && first_bit_mismatch_except(&actual_solver_y, expected_solver_y, &state_slots)
                .is_none()
            && self.frozen_parameters_match(expected_parameters)
        {
            return Ok(());
        }
        let solver_mismatch =
            first_bit_mismatch_except(&actual_solver_y, expected_solver_y, &state_slots);
        let parameter_mismatch = first_bit_mismatch_except(
            &self.params,
            expected_parameters,
            &self
                .runtime
                .model
                .problem
                .events
                .delays
                .value_parameter_indices,
        );
        let solver_name = solver_mismatch.and_then(|(index, _, _)| {
            self.runtime
                .model
                .problem
                .solve_layout
                .solver_maps
                .names
                .get(index)
        });
        Err(contract(format!(
            "frozen compatibility state diverged at {stage:?}: component_time={} \
             last_event={:?} component_solver_y={} \
             expected_solver_y={} component_parameters={} expected_parameters={} \
             solver_mismatch={solver_mismatch:?} solver_name={solver_name:?} \
             parameter_mismatch={parameter_mismatch:?}",
            self.time,
            self.last_event_entry,
            actual_solver_y.len(),
            expected_solver_y.len(),
            self.params.len(),
            expected_parameters.len(),
        ))
        .at_stage(stage))
    }

    pub(super) fn frozen_parameters_match(&self, expected: &[f64]) -> bool {
        self.params.len() == expected.len()
            && self
                .params
                .iter()
                .zip(expected)
                .enumerate()
                .all(|(index, (actual, expected))| {
                    self.runtime
                        .model
                        .problem
                        .events
                        .delays
                        .value_parameter_indices
                        .contains(&index)
                        || actual.to_bits() == expected.to_bits()
                })
    }

    pub(super) fn instantiate_inner(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
    ) -> Result<Self, MeError> {
        validate_instance_config(config)?;
        let model = source.model();
        rumoca_eval_solve::reset_solve_row_eval_trace();
        validate_explicit_solve_model(model)?;
        let model = model
            .resolved_periodic_schedules_at(config.start_time)
            .map_err(|error| {
                contract(format!(
                    "periodic schedule cannot be anchored at FMI startTime: {error}"
                ))
            })?;
        let runtime = Rc::new(SolveRuntime::new(&model)?);
        let state_count = runtime.state_count;
        let states = runtime.model.initial_y[..state_count].to_vec();
        let params = runtime.model.parameters.clone();
        let stop_schedule =
            SolveStopSchedule::new(&runtime.model.problem, config.start_time, config.stop_time);
        let output_meta = convert_variable_meta(&runtime.model.variable_meta);
        Ok(Self {
            solver_y_guess: RefCell::new(runtime.model.initial_y.clone()),
            delay_params_scratch: RefCell::new(params.clone()),
            delay_solver_y_scratch: RefCell::new(runtime.model.initial_y.clone()),
            runtime,
            instance_brand: Rc::new(()),
            instance_name: config.instance_name,
            lifecycle: MeLifecycle::instantiated(),
            tolerance: config.tolerance,
            stop_time: config.stop_time,
            root_profile: config.root_profile,
            numerics_profile: config.numerics_profile,
            time: config.start_time,
            event_boundary: None,
            post_event_eval_time: None,
            event_anchor_time: config.start_time,
            states,
            params,
            state_count,
            stop_schedule,
            pending_event_entry: None,
            last_event_entry: None,
            pending_event_stop: None,
            advance_state_to_event_right_limit: false,
            state_time_coincidence: StateTimeCoincidence::None,
            initial_event_pending: false,
            skip_next_enter_continuous_delay_commit: false,
            pending_root_crossings: Vec::new(),
            frozen_event_root_crossings: Vec::new(),
            pending_event_pre_y: None,
            pending_event_pre_p: None,
            boundary_event_pre_y: None,
            boundary_event_pre_p: None,
            frozen_event_accepted_seed: None,
            derivative_cache: RefCell::new(None),
            root_cache: RefCell::new(None),
            initial_observations: Vec::new(),
            delay_step_limit: None,
            last_projection_changed: false,
            termination: None,
            output_meta,
            settled_initialization_y: None,
        })
    }

    // -- internal time model ---------------------------------------------

    /// The evaluation time a variable read at the component's current time
    /// uses: after an event with a right limit, the right limit itself.
    pub(super) fn public_time_eval_time(&self, time: f64) -> f64 {
        match self.post_event_eval_time {
            Some(eval_time) if time_match_with_tol(time, self.event_anchor_time) => eval_time,
            _ => time,
        }
    }

    /// The evaluation time derivative and event-indicator reads use.
    pub(super) fn continuous_eval_time(&self) -> f64 {
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return self.time;
        }
        match self.event_boundary {
            Some(boundary) if self.time >= boundary => {
                timeline::event_left_probe_time(boundary, self.tolerance)
            }
            _ => self.public_time_eval_time(self.time),
        }
    }

    pub(super) fn set_post_event_eval_time(&mut self, right_limit: Option<f64>) {
        self.post_event_eval_time = right_limit;
        self.event_anchor_time = self.time;
    }

    pub(super) fn numerics_settle(&self) -> AlgebraicSettle {
        match self.numerics_profile {
            MeNumericsProfile::Component => AlgebraicSettle {
                tol: ALGEBRAIC_REFRESH_TOL,
                max_iters: UPDATE_MAX_ITERS,
            },
            MeNumericsProfile::DiffsolFrozen => AlgebraicSettle {
                tol: self.tolerance.max(1.0e-10),
                max_iters: 256,
            },
        }
    }

    pub(super) fn algebraic_projection_policy(&self) -> MeAlgebraicProjectionPolicy {
        MeAlgebraicProjectionPolicy {
            state_count: self.state_count,
            tolerance: self.tolerance,
            profile: self.numerics_profile,
            settle: self.numerics_settle(),
        }
    }

    pub(super) fn initialization_solver_y(&self) -> Result<Vec<f64>, MeError> {
        match self.numerics_profile {
            MeNumericsProfile::Component => self.current_solver_y(),
            // The frozen Diffsol initialization starts from the declared
            // full-layout seed exactly as the retired driver did.  A
            // preliminary full refresh would prime runtime-owned evaluation
            // state in a different order even when its returned vector is
            // later overwritten by the initialization solve.
            MeNumericsProfile::DiffsolFrozen => Ok(self.solver_y_guess.borrow().clone()),
        }
    }

    pub(super) fn with_callback_solver_y<R>(&self, f: impl FnOnce(&mut Vec<f64>) -> R) -> R {
        match self.numerics_profile {
            MeNumericsProfile::Component => f(&mut self.solver_y_guess.borrow_mut()),
            MeNumericsProfile::DiffsolFrozen => {
                let mut speculative = self.solver_y_guess.borrow().clone();
                f(&mut speculative)
            }
        }
    }

    // -- internal solver vector ------------------------------------------

    pub(super) fn current_solver_y(&self) -> Result<Vec<f64>, MeError> {
        self.solver_y_at_time(self.public_time_eval_time(self.time))
    }

    pub(super) fn solver_y_at_time(&self, time: f64) -> Result<Vec<f64>, MeError> {
        let settle = self.numerics_settle();
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.with_callback_solver_y(|guess| {
                self.runtime
                    .full_solver_y_with_guess(
                        time,
                        &self.states,
                        params,
                        guess,
                        settle.tol,
                        settle.max_iters,
                    )
                    .map(|()| guess.clone())
                    .map_err(MeError::from)
            })
        })?
    }

    pub(super) fn copy_states_from_solver_y(&mut self, solver_y: &[f64]) {
        for (dst, src) in self.states.iter_mut().zip(solver_y.iter().copied()) {
            *dst = src;
        }
    }

    pub(super) fn with_delay_evaluation_params<R>(
        &self,
        time: f64,
        state: &[f64],
        f: impl FnOnce(&[f64]) -> R,
    ) -> Result<R, MeError> {
        if !self.runtime.has_delay_channels() {
            return Ok(f(&self.params));
        }
        let mut params = self.delay_params_scratch.borrow_mut();
        params.resize(self.params.len(), 0.0);
        params.copy_from_slice(&self.params);
        let mut solver_y = self.delay_solver_y_scratch.borrow_mut();
        {
            let guess = self.solver_y_guess.borrow();
            solver_y.resize(guess.len(), 0.0);
            solver_y.copy_from_slice(&guess);
        }
        if solver_y.len() < state.len() {
            return Err(contract(format!(
                "delay evaluation solver vector has {} entries for {} state values",
                solver_y.len(),
                state.len()
            )));
        }
        solver_y[..state.len()].copy_from_slice(state);
        self.runtime
            .refresh_delay_values(time, &solver_y, &mut params)?;
        Ok(f(&params))
    }

    pub(super) fn commit_delay_point(&mut self) -> Result<(), MeError> {
        if !self.runtime.has_delay_channels() {
            return Ok(());
        }
        let settle = self.numerics_settle();
        let mut solver_y = self.solver_y_guess.borrow_mut();
        if solver_y.len() < self.states.len() {
            return Err(contract(format!(
                "delay commit solver vector has {} entries for {} state values",
                solver_y.len(),
                self.states.len()
            )));
        }
        solver_y[..self.states.len()].copy_from_slice(&self.states);
        self.delay_step_limit =
            self.runtime
                .refresh_delay_values(self.time, &solver_y, &mut self.params)?;
        self.runtime.full_solver_y_with_guess(
            self.time,
            &self.states,
            &self.params,
            &mut solver_y,
            settle.tol,
            settle.max_iters,
        )?;
        self.runtime
            .commit_delay_history(self.time, &solver_y, &self.params)?;
        Ok(())
    }

    // -- caches ------------------------------------------------------------

    pub(super) fn cached_derivative(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return None;
        }
        let cache = self.derivative_cache.borrow();
        let cached = cache.as_ref()?;
        if !time_match_with_tol(cached.time, time) || !state_values_match(&cached.state, state) {
            return None;
        }
        Some(cached.derivative.clone())
    }

    pub(super) fn cache_derivative(&self, time: f64, state: &[f64], derivative: &[f64]) {
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return;
        }
        *self.derivative_cache.borrow_mut() = Some(CachedDerivative {
            time,
            state: state.to_vec(),
            derivative: derivative.to_vec(),
        });
    }

    pub(super) fn clear_derivative_cache(&self) {
        *self.derivative_cache.borrow_mut() = None;
    }

    pub(super) fn clear_runtime_caches(&self) {
        self.clear_derivative_cache();
        *self.root_cache.borrow_mut() = None;
    }

    pub(super) fn cached_root_conditions(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return None;
        }
        let cache = self.root_cache.borrow();
        let cached = cache.as_ref()?;
        if !time_match_with_tol(cached.time, time) || !state_values_match(&cached.state, state) {
            return None;
        }
        Some(cached.values.clone())
    }

    pub(super) fn cache_root_conditions(&self, time: f64, state: &[f64], values: &[f64]) {
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return;
        }
        *self.root_cache.borrow_mut() = Some(CachedRootConditions {
            time,
            state: state.to_vec(),
            values: values.to_vec(),
        });
    }

    // -- initialization ----------------------------------------------------

    /// `fmi3EnterInitializationMode`, unannotated; the trait method attaches
    /// [`MeStage::Initialization`].
    pub(super) fn enter_initialization_mode_inner(&mut self) -> Result<(), MeError> {
        self.runtime.initialize_delay_history(
            self.time,
            &self.runtime.model.initial_y,
            &mut self.params,
        )?;
        self.runtime.set_initial_event_flag(&mut self.params, true);
        Ok(())
    }

    /// `fmi3ExitInitializationMode`, unannotated; the trait method attaches
    /// [`MeStage::Initialization`].
    pub(super) fn exit_initialization_mode_inner(&mut self) -> Result<(), MeError> {
        let mut solver_y = self.initialization_solver_y()?;
        let policy = self.algebraic_projection_policy();
        let settle = policy.settle;
        match self.numerics_profile {
            MeNumericsProfile::Component => {
                self.runtime.settle_initialization_system(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                project_algebraics(
                    &self.runtime,
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    policy,
                )?;
                self.copy_states_from_solver_y(&solver_y);
                self.runtime.update_relation_memory_from_state(
                    self.time,
                    &self.states,
                    &mut self.params,
                    self.tolerance,
                    settle.max_iters,
                )?;
            }
            MeNumericsProfile::DiffsolFrozen => {
                self.runtime.seed_initial_discrete_values(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                self.runtime
                    .settle_runtime_assignments_and_relation_memory(
                        &mut solver_y,
                        &mut self.params,
                        self.time,
                        self.tolerance,
                        settle.max_iters,
                    )?;
                self.runtime.settle_initialization_system(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                self.runtime.seed_initial_discrete_values(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                self.runtime.settle_initialization_system(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                let runtime = Rc::clone(&self.runtime);
                let projection_runtime = Rc::clone(&runtime);
                let tol = policy.tolerance;
                let time = self.time;
                runtime.settle_projected_runtime_and_relation_memory(
                    &mut solver_y,
                    &mut self.params,
                    time,
                    tol,
                    settle.max_iters,
                    move |y, p| project_algebraics(&projection_runtime, y, p, time, policy),
                )?;
            }
        }
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y.clone();
        // MLS 3.6 §8.6: before integration, v = pre(v). The initial event
        // therefore reads the values the initialization system just settled,
        // never the declared starts that seeded that solve.
        self.pending_event_pre_y = Some(solver_y.clone());
        self.pending_event_pre_p = Some(self.params.clone());
        self.settled_initialization_y = Some(solver_y);
        self.initial_event_pending = true;
        Ok(())
    }

    // -- continuous time mode ----------------------------------------------

    /// [`ModelExchangeKernel::project_continuous_states`], unannotated; the
    /// trait method attaches [`MeStage::ManifoldProjection`].
    pub(super) fn project_continuous_states_inner(
        &mut self,
        states: &mut [f64],
    ) -> Result<bool, MeError> {
        let time = self.time;
        let settle = self.numerics_settle();
        let (mut solver_y, accepted_guess) = match self.numerics_profile {
            MeNumericsProfile::Component => (
                self.runtime.full_solver_y(
                    time,
                    states,
                    &self.params,
                    settle.tol,
                    settle.max_iters,
                )?,
                None,
            ),
            MeNumericsProfile::DiffsolFrozen => {
                let accepted_guess = self.solver_y_guess.borrow().clone();
                let mut projection_guess = accepted_guess.clone();
                self.runtime.full_solver_y_with_guess(
                    time,
                    states,
                    &self.params,
                    &mut projection_guess,
                    settle.tol,
                    settle.max_iters,
                )?;
                (projection_guess, Some(accepted_guess))
            }
        };
        let changed = self.runtime.project_state_manifold(
            &mut solver_y,
            &self.params,
            time,
            self.tolerance,
        )?;
        states.copy_from_slice(&solver_y[..self.state_count]);
        let mut committed_guess = accepted_guess.unwrap_or(solver_y);
        self.runtime.full_solver_y_with_guess(
            time,
            states,
            &self.params,
            &mut committed_guess,
            settle.tol,
            settle.max_iters,
        )?;
        *self.solver_y_guess.borrow_mut() = committed_guess;
        self.last_projection_changed = changed;
        Ok(changed)
    }

    /// [`ModelExchangeKernel::next_event_stop`], unannotated; the trait method
    /// attaches [`MeStage::Integration`].
    pub(super) fn next_event_stop_inner(&mut self, horizon: f64) -> Result<MeEventStop, MeError> {
        let solver_y = self.current_solver_y()?;
        let (time, event) = self.runtime.next_runtime_event_stop(
            &solver_y,
            &self.params,
            &mut self.stop_schedule,
            self.time,
            horizon,
        )?;
        self.pending_event_stop = event.map(|event| (time, event));
        Ok(MeEventStop {
            time,
            is_event: event.is_some(),
        })
    }

    pub fn has_scheduled_event_at(&self, time: f64) -> bool {
        self.stop_schedule
            .scheduled_event_coincidence_at(time)
            .is_some()
            || self
                .pending_event_stop
                .is_some_and(|(event_time, _)| time_match_with_tol(event_time, time))
    }

    pub fn frozen_event_state_derivatives(
        &self,
        time: f64,
        states: &[f64],
    ) -> Result<Vec<f64>, MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Err(contract(
                "frozen event derivative evaluation requires DiffsolFrozen numerics",
            )
            .at_stage(MeStage::EventIteration));
        }
        self.runtime
            .eval_state_derivatives(time, states, &self.params, self.tolerance.max(1.0e-10), 256)
            .map_err(MeError::from)
            .map_err(|error| error.at_stage(MeStage::EventIteration))
    }

    /// Freeze the retired driver's full-vector ownership at a located root.
    ///
    /// The driver reconstructs every solver lane at the located state, then
    /// brackets the event by changing only the continuous-state prefix.  In
    /// particular, algebraic lanes in the left-limit snapshot still belong to
    /// the located root rather than to a fresh solve at the extrapolated left
    /// state.  This temporary phase-2 bridge preserves that exact ownership.
    pub fn capture_frozen_located_event_pre(&mut self, pre_states: &[f64]) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Err(
                contract("frozen located-event capture requires DiffsolFrozen numerics")
                    .at_stage(MeStage::EventIteration),
            );
        }
        if pre_states.len() != self.state_count {
            return Err(contract(format!(
                "frozen located-event pre-state has {} entries for {} continuous states",
                pre_states.len(),
                self.state_count
            ))
            .at_stage(MeStage::EventIteration));
        }
        let mut event_pre_y = self.solver_y_at_time(self.time)?;
        event_pre_y[..self.state_count].copy_from_slice(pre_states);
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(self.params.clone());
        Ok(())
    }

    pub fn prepare_frozen_bdf_initial_seed(
        &mut self,
        frozen_solver_y: &[f64],
    ) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Err(contract(
                "frozen BDF seed preparation requires DiffsolFrozen numerics",
            ));
        }
        if frozen_solver_y.len() != self.runtime.solver_count {
            return Err(contract(format!(
                "frozen BDF seed has {} entries for {} solver values",
                frozen_solver_y.len(),
                self.runtime.solver_count
            )));
        }
        self.solver_y_guess
            .borrow_mut()
            .copy_from_slice(frozen_solver_y);
        Ok(())
    }

    // -- event boundary ----------------------------------------------------

    pub(super) fn apply_discrete_event_updates(
        &mut self,
        event_time: f64,
        _event: RuntimeEventStop,
        row_filter: EventUpdateRowFilter,
    ) -> Result<(), MeError> {
        let event_entry_y = self
            .pending_event_pre_y
            .take()
            .map(Ok)
            .unwrap_or_else(|| self.current_solver_y())?;
        let event_entry_p = self
            .pending_event_pre_p
            .take()
            .unwrap_or_else(|| self.params.clone());
        let mut solver_y = self.event_iteration_solver_y(&event_entry_y)?;
        let pending_root_crossings = self.pending_root_crossings.drain(..).collect::<Vec<_>>();
        let pending_root_overrides = pending_root_crossings
            .iter()
            .map(|crossing| (crossing.index, crossing.post_relation_memory_value))
            .collect::<Vec<_>>();
        let has_typed_root_override = pending_root_overrides.iter().any(|(index, _)| {
            matches!(
                self.runtime
                    .model
                    .problem
                    .events
                    .root_relation_memory_targets
                    .get(*index),
                Some(Some(_))
            )
        });
        let root_overrides = match self.numerics_profile {
            MeNumericsProfile::Component => pending_root_overrides.as_slice(),
            MeNumericsProfile::DiffsolFrozen if has_typed_root_override => {
                pending_root_overrides.as_slice()
            }
            MeNumericsProfile::DiffsolFrozen => &[],
        };
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            let typed_crossings = pending_root_crossings
                .into_iter()
                .filter(|crossing| {
                    matches!(
                        self.runtime
                            .model
                            .problem
                            .events
                            .root_relation_memory_targets
                            .get(crossing.index),
                        Some(Some(_))
                    )
                })
                .collect::<Vec<_>>();
            if !typed_crossings.is_empty() {
                self.frozen_event_root_crossings = typed_crossings;
            }
        }
        let runtime = Rc::clone(&self.runtime);
        let projection_runtime = Rc::clone(&runtime);
        let settle_projection_runtime = Rc::clone(&runtime);
        let policy = self.algebraic_projection_policy();
        let tol = policy.tolerance;
        let settle = policy.settle;
        let outcome = runtime.apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut solver_y,
                p: &mut self.params,
                t: event_time,
                tol,
                event_pre_y: &event_entry_y,
                event_pre_p: &event_entry_p,
                max_iters: settle.max_iters,
                row_filter,
                root_relation_overrides: root_overrides,
            },
            move |y, p| project_algebraics(&projection_runtime, y, p, event_time, policy),
        )?;
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen)
            && !has_typed_root_override
        {
            // The frozen compatibility settle reconstructs relation memory
            // from the numerical application point. A located crossing has a
            // stronger, typed post-side value that the event iteration above
            // has already settled; recomputing at the exact root would erase
            // that value for strict relations.
            runtime.settle_projected_runtime_and_relation_memory(
                &mut solver_y,
                &mut self.params,
                event_time,
                tol,
                settle.max_iters,
                move |y, p| {
                    project_algebraics(&settle_projection_runtime, y, p, event_time, policy)
                },
            )?;
        }
        self.commit_event_runtime_state(event_time, solver_y, root_overrides)?;
        self.record_event_action_outcome(outcome, event_time)?;
        self.clear_runtime_caches();
        Ok(())
    }

    pub(super) fn commit_event_runtime_state(
        &mut self,
        event_time: f64,
        mut solver_y: Vec<f64>,
        root_overrides: &[(usize, f64)],
    ) -> Result<(), MeError> {
        if matches!(self.numerics_profile, MeNumericsProfile::Component) {
            let history_changed = commit_pre_params_after_event_at(
                &self.runtime.model,
                &solver_y,
                &mut self.params,
                Some(event_time),
                self.tolerance,
            );
            if history_changed {
                self.canonicalize_committed_event_view(event_time, &mut solver_y, root_overrides)?;
            }
        }
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y;
        if matches!(self.numerics_profile, MeNumericsProfile::Component) {
            self.commit_delay_point()?;
        }
        Ok(())
    }

    pub(super) fn event_iteration_solver_y(
        &self,
        event_entry_y: &[f64],
    ) -> Result<Vec<f64>, MeError> {
        match self.numerics_profile {
            MeNumericsProfile::Component => self.current_solver_y(),
            // The frozen driver starts a located event from its dense-output
            // full vector, then replaces only the continuous-state prefix when
            // bracketing the right limit. Preserve that ownership here: a
            // tolerance-equal root may snap back to an output target, and
            // rebuilding every lane at that target can change strict relation
            // memory before the shared event iteration sees the located side.
            MeNumericsProfile::DiffsolFrozen => {
                let mut solver_y = event_entry_y.to_vec();
                solver_y[..self.state_count].copy_from_slice(&self.states);
                Ok(solver_y)
            }
        }
    }

    pub(super) fn finish_frozen_runtime_event(&mut self, event_time: f64) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Ok(());
        }
        let mut post_event_y = self.current_solver_y()?;
        let root_overrides = std::mem::take(&mut self.frozen_event_root_crossings)
            .into_iter()
            .map(|crossing| (crossing.index, crossing.post_relation_memory_value))
            .collect::<Vec<_>>();
        self.settle_frozen_pre_commit_event_view(event_time, &mut post_event_y, &root_overrides)?;
        let history_changed = commit_pre_params_after_event_at(
            &self.runtime.model,
            &post_event_y,
            &mut self.params,
            Some(event_time),
            self.tolerance,
        );
        if history_changed {
            let mut canonical_y = post_event_y;
            self.canonicalize_committed_event_view(event_time, &mut canonical_y, &root_overrides)?;
        }
        self.commit_delay_point()?;
        if let Some(accepted_seed) = self.frozen_event_accepted_seed.take() {
            *self.solver_y_guess.borrow_mut() = accepted_seed;
        }
        self.skip_next_enter_continuous_delay_commit = true;
        Ok(())
    }

    /// Reconcile the reconstructed full solver view while event `pre` is
    /// still frozen. Relation-evaluating B.1c owners may run only here; after
    /// history commits, canonicalization consumes the certified post plan.
    pub(super) fn settle_frozen_pre_commit_event_view(
        &mut self,
        event_time: f64,
        solver_y: &mut [f64],
        root_relation_overrides: &[(usize, f64)],
    ) -> Result<(), MeError> {
        let runtime = Rc::clone(&self.runtime);
        let policy = self.algebraic_projection_policy();
        for _ in 0..policy.settle.max_iters {
            let before_y = solver_y.to_vec();
            let before_p = self.params.clone();
            runtime.apply_runtime_assignments_until_stable(
                solver_y,
                &mut self.params,
                event_time,
                policy.settle.tol,
                policy.settle.max_iters,
            )?;
            project_algebraics(&runtime, solver_y, &mut self.params, event_time, policy)?;
            runtime.update_algebraic_relation_memory_from_solver_y_except_overrides(
                event_time,
                solver_y,
                &mut self.params,
                root_relation_overrides,
            )?;
            if !runtime_values_changed(&before_y, solver_y, policy.settle.tol)
                && !runtime_values_changed(&before_p, &self.params, policy.settle.tol)
            {
                return Ok(());
            }
        }
        Err(contract(format!(
            "pre-commit derived event view did not converge at t={event_time}"
        )))
    }

    /// Reconstruct the canonical post-event view after `pre` history advances.
    ///
    /// This deliberately settles runtime assignments, algebraic projection,
    /// and typed root relation memory. Discrete event rows are not replayed:
    /// they already completed their one Appendix-B event iteration.
    pub(super) fn canonicalize_committed_event_view(
        &mut self,
        event_time: f64,
        solver_y: &mut [f64],
        root_relation_overrides: &[(usize, f64)],
    ) -> Result<(), MeError> {
        let runtime = Rc::clone(&self.runtime);
        let policy = self.algebraic_projection_policy();
        // Relation-free derived values are safe after `pre` commits and remain
        // the only discrete owners admitted to this coupled loop. Owners that
        // evaluate relations already settled during event iteration while
        // `pre` was frozen and must not be replayed here. Algebraic-dependent
        // relation memory refreshes from the projected canonical view;
        // parameter-only relation memory remains on its selected event side.
        runtime.apply_post_commit_assignments_until_stable(
            solver_y,
            &mut self.params,
            event_time,
            policy.settle.tol,
            policy.settle.max_iters,
        )?;
        project_algebraics(&runtime, solver_y, &mut self.params, event_time, policy)?;
        runtime.update_algebraic_relation_memory_from_solver_y_except_overrides(
            event_time,
            solver_y,
            &mut self.params,
            root_relation_overrides,
        )?;
        project_algebraics(&runtime, solver_y, &mut self.params, event_time, policy)?;
        for _ in 0..policy.settle.max_iters {
            let before_y = solver_y.to_vec();
            let before_p = self.params.clone();
            runtime.apply_post_commit_assignments_until_stable(
                solver_y,
                &mut self.params,
                event_time,
                policy.settle.tol,
                policy.settle.max_iters,
            )?;
            project_algebraics(&runtime, solver_y, &mut self.params, event_time, policy)?;
            runtime.update_algebraic_relation_memory_from_solver_y_except_overrides(
                event_time,
                solver_y,
                &mut self.params,
                root_relation_overrides,
            )?;
            if !runtime_values_changed(&before_y, solver_y, policy.settle.tol)
                && !runtime_values_changed(&before_p, &self.params, policy.settle.tol)
            {
                return Ok(());
            }
        }
        Err(contract(format!(
            "post-commit derived event view did not converge at t={event_time}"
        )))
    }

    pub(super) fn complete_coincident_root_right_limit(
        &mut self,
        entry: MeEventEntry,
        event: RuntimeEventStop,
        settled_right_limit: Option<f64>,
        tolerance: f64,
    ) -> Result<Option<f64>, MeError> {
        let right_time =
            runtime_root_event_application_time(entry.event_time, entry.horizon, tolerance);
        if settled_right_limit.map(f64::to_bits) == Some(right_time.to_bits()) {
            return Ok(settled_right_limit);
        }
        // The clock owner has completed at the semantic tick. The root's
        // numerical right-limit transition starts from that settled superdense
        // value and may execute only unowned rows; clock-owned rows cannot
        // sample the post-event state a second time.
        let event_pre_y = self.current_solver_y()?;
        self.boundary_event_pre_y = Some(event_pre_y);
        self.boundary_event_pre_p = Some(self.params.clone());
        RuntimeEventBoundaryHandler::on_event_right_limit(self, right_time, event)?;
        Ok(Some(right_time))
    }

    pub(super) fn refresh_frozen_event_observation(&mut self, time: f64) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Ok(());
        }
        let mut solver_y = self.solver_y_guess.borrow().clone();
        self.runtime
            .refresh_delay_values(time, &solver_y, &mut self.params)?;
        self.runtime.refresh_observation_discrete_rows(
            &mut solver_y,
            &mut self.params,
            time,
            self.tolerance.max(1.0e-10),
            256,
        )?;
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y;
        Ok(())
    }

    pub(super) fn record_event_action_outcome(
        &mut self,
        outcome: EventActionOutcome,
        event_time: f64,
    ) -> Result<(), MeError> {
        match outcome {
            EventActionOutcome::Continue => Ok(()),
            EventActionOutcome::AssertionFailed { time, message } => Err(MeError::Assertion {
                time: if time.is_finite() { time } else { event_time },
                message,
            }),
            EventActionOutcome::Terminated { time, message } => {
                let time = if time.is_finite() { time } else { event_time };
                self.termination
                    .get_or_insert(SimTermination { time, message });
                Ok(())
            }
        }
    }

    pub(super) fn event_pre_for_update(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(Vec<f64>, Vec<f64>), MeError> {
        if let Some(mut event_pre_y) = self.pending_event_pre_y.take() {
            if matches!(
                self.state_time_coincidence,
                StateTimeCoincidence::Unconsumed
            ) {
                // A located-root snapshot brackets the root with a
                // tolerance-wide probe so relation memory can classify its
                // post side. When a typed clock owns the same superdense
                // instant, that numerical probe is not the clock's semantic
                // `pre`: continuous states belong to the shared event-entry
                // point supplied by the importer. Preserve the located
                // non-state lanes required by the frozen profile, but restore
                // the continuous-state prefix before clock-owned rows sample
                // it.
                event_pre_y[..self.state_count].copy_from_slice(&self.states);
            }
            let event_pre_p = self
                .pending_event_pre_p
                .take()
                .unwrap_or_else(|| self.params.clone());
            return Ok((event_pre_y, event_pre_p));
        }
        let pre_time = match event.pre_mode {
            EventPreMode::EventEntry | EventPreMode::Fixed => {
                timeline::event_left_probe_time(event_time, self.tolerance)
            }
            EventPreMode::FollowCurrent => self.public_time_eval_time(self.time),
        };
        let event_pre_y = self.solver_y_at_time(pre_time)?;
        let event_pre_p = self.params.clone();
        Ok((event_pre_y, event_pre_p))
    }

    pub(super) fn clear_event_entry_scheduled_root_relation_memory(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), MeError> {
        if event.observe_right_limit || !matches!(event.pre_mode, EventPreMode::EventEntry) {
            return Ok(());
        }
        let root_indices = self.scheduled_root_indices_at_time(event_time);
        self.clear_scheduled_root_relation_memory(&root_indices)
    }

    pub(super) fn clear_all_scheduled_root_relation_memory(&mut self) -> Result<(), MeError> {
        let root_indices = self
            .runtime
            .model
            .problem
            .events
            .scheduled_root_conditions
            .iter()
            .map(|root| root.root_index)
            .collect::<Vec<_>>();
        self.clear_scheduled_root_relation_memory(&root_indices)
    }

    pub(super) fn clear_scheduled_root_relation_memory(
        &mut self,
        root_indices: &[usize],
    ) -> Result<(), MeError> {
        clear_scheduled_root_relation_memory(&self.runtime.model, root_indices, &mut self.params)
            .map_err(contract)
    }

    pub(super) fn seed_scheduled_root_relation_overrides(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) {
        if event.observe_right_limit || !matches!(event.pre_mode, EventPreMode::EventEntry) {
            return;
        }
        for index in self.scheduled_root_indices_at_time(event_time) {
            self.pending_root_crossings.push(RootCrossing {
                index,
                post_relation_memory_value: 1.0,
            });
        }
    }

    pub(super) fn scheduled_root_indices_at_time(&self, event_time: f64) -> Vec<usize> {
        timeline::scheduled_root_indices_at_time(
            &self.runtime.model.problem.events.scheduled_root_conditions,
            event_time,
        )
    }

    pub(super) fn run_initial_event_boundary(&mut self) -> Result<MeDiscreteStates, MeError> {
        let continuous_states_before = self.states.clone();
        let event_time = self.time;
        let mut solver_y = self
            .settled_initialization_y
            .take()
            .ok_or_else(|| contract("initial event boundary requires a settled solver vector"))?;
        let startup_event_pre_y = self
            .pending_event_pre_y
            .take()
            .ok_or_else(|| contract("initial event boundary requires a latched pre-event state"))?;
        let startup_event_pre_p = self
            .pending_event_pre_p
            .take()
            .unwrap_or_else(|| self.params.clone());
        let dynamic_event =
            self.runtime
                .current_dynamic_time_event_stop(&solver_y, &self.params, self.time)?;
        let runtime = Rc::clone(&self.runtime);
        let projection_runtime = Rc::clone(&runtime);
        let policy = self.algebraic_projection_policy();
        let tol = policy.tolerance;
        let settle = policy.settle;
        let outcome = runtime.apply_projected_initial_event_boundary(
            ProjectedInitialEventInput {
                y: &mut solver_y,
                p: &mut self.params,
                t_start: self.time,
                t_end: self.stop_time,
                tol,
                event_pre_y: &startup_event_pre_y,
                event_pre_p: &startup_event_pre_p,
                max_iters: settle.max_iters,
                dynamic_event,
                apply_without_initial_event: self.root_profile.apply_without_initial_event(),
            },
            move |y, p, t| project_algebraics(&projection_runtime, y, p, t, policy),
        )?;
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y;
        self.time = outcome.final_t;
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen)
            && self.runtime.has_delay_channels()
        {
            let solver_y = self.solver_y_guess.borrow();
            self.runtime
                .commit_delay_history(self.time, &solver_y, &self.params)?;
            self.skip_next_enter_continuous_delay_commit = true;
        }
        self.initial_observations = outcome
            .observations
            .iter()
            .map(|observation| observation_from_initial_event(observation, &self.instance_brand))
            .collect();
        self.record_event_action_outcome(outcome.action, event_time)?;
        self.initial_event_pending = false;
        let right_limit = (outcome.final_t > event_time).then_some(outcome.final_t);
        self.time = event_time;
        self.set_post_event_eval_time(right_limit);
        self.discrete_states_after_update(continuous_state_values_changed(
            &continuous_states_before,
            &self.states,
        ))
    }

    pub(super) fn run_runtime_event_boundary(
        &mut self,
        entry: MeEventEntry,
    ) -> Result<MeDiscreteStates, MeError> {
        let continuous_states_before = self.states.clone();
        let tolerance = self.tolerance.max(1.0e-10);
        match entry.cause {
            MeEventCause::StateEvent => {
                self.advance_state_to_event_right_limit = false;
                let scheduled = self
                    .stop_schedule
                    .scheduled_event_coincidence_at(entry.event_time);
                let coincident_time_event = scheduled
                    .map(|coincidence| (coincidence.event.time, coincidence.event.event))
                    .or_else(|| {
                        self.pending_event_stop
                            .filter(|(time, _)| time_match_with_tol(*time, entry.event_time))
                    });
                self.state_time_coincidence = match scheduled.map(|value| value.consumption) {
                    Some(ScheduledEventConsumption::Unconsumed) => StateTimeCoincidence::Unconsumed,
                    Some(ScheduledEventConsumption::Consumed) => StateTimeCoincidence::Consumed,
                    None if coincident_time_event.is_some() => StateTimeCoincidence::Unconsumed,
                    None => StateTimeCoincidence::None,
                };
                let (event_time, event) = coincident_time_event.unwrap_or_else(|| {
                    (
                        entry.event_time,
                        RuntimeEventStop::static_event(EventPreMode::EventEntry),
                    )
                });
                let horizon_t = coincident_time_event
                    .map_or(entry.event_time.min(entry.horizon), |(_, event)| {
                        runtime_event_horizon(event, entry.horizon, self.stop_time)
                    });
                let outcome = process_runtime_event_boundary(
                    RuntimeEventBoundary {
                        event_t: event_time,
                        horizon_t,
                        tolerance,
                        event,
                    },
                    self,
                )?;
                let mut right_limit_t = outcome.right_limit_t;
                if coincident_time_event.is_some()
                    && matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen)
                {
                    right_limit_t = self.complete_coincident_root_right_limit(
                        entry,
                        event,
                        outcome.right_limit_t,
                        tolerance,
                    )?;
                }
                self.finish_frozen_runtime_event(entry.event_time)?;
                if coincident_time_event.is_some() {
                    self.stop_schedule.advance_past(event_time);
                    self.pending_event_stop = None;
                    self.set_post_event_eval_time(right_limit_t);
                    self.clear_event_entry_scheduled_root_relation_memory(outcome.final_t, event)?;
                    self.clear_runtime_caches();
                }
                self.state_time_coincidence = StateTimeCoincidence::None;
                self.discrete_states_after_update(continuous_state_values_changed(
                    &continuous_states_before,
                    &self.states,
                ))
            }
            MeEventCause::TimeEvent => {
                self.advance_state_to_event_right_limit = true;
                self.state_time_coincidence = StateTimeCoincidence::None;
                let (_, event) = self.pending_event_stop.take().ok_or_else(|| {
                    contract("time event entered without a scheduled component event")
                })?;
                let outcome = process_runtime_event_boundary(
                    RuntimeEventBoundary {
                        event_t: entry.event_time,
                        horizon_t: runtime_event_horizon(event, entry.horizon, self.stop_time),
                        tolerance,
                        event,
                    },
                    self,
                )?;
                self.advance_state_to_event_right_limit = false;
                self.finish_frozen_runtime_event(entry.event_time)?;
                self.stop_schedule.advance_past(entry.event_time);
                self.set_post_event_eval_time(outcome.right_limit_t);
                self.clear_event_entry_scheduled_root_relation_memory(outcome.final_t, event)?;
                self.clear_runtime_caches();
                self.discrete_states_after_update(continuous_state_values_changed(
                    &continuous_states_before,
                    &self.states,
                ))
            }
        }
    }

    /// Build the exact `fmi3UpdateDiscreteStates` output set after the event
    /// iteration has settled. Time remains importer-owned; the next scheduled
    /// event is announced here rather than exposed through a second component
    /// scheduling operation.
    pub(super) fn discrete_states_after_update(
        &mut self,
        values_of_continuous_states_changed: bool,
    ) -> Result<MeDiscreteStates, MeError> {
        let next_event_time = if self.termination.is_some() || self.time >= self.stop_time {
            self.pending_event_stop = None;
            None
        } else {
            let stop = self.next_event_stop_inner(self.stop_time)?;
            stop.is_event.then_some(stop.time)
        };
        Ok(MeDiscreteStates {
            discrete_states_need_update: false,
            terminate_simulation: self.termination.clone(),
            values_of_continuous_states_changed,
            nominals_of_continuous_states_changed: false,
            next_event_time,
        })
    }
}
