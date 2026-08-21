use rumoca_ir_solve::{self as solve, ScalarSlot};

use super::event_boundary::event_boundary_horizon;
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
        if self.copy_cached_derivative_into(time, &self.states, derivatives) {
            return Ok(());
        }
        let settle = self.numerics_settle();
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.state_derivatives_at_parameters(time, params, settle, derivatives)
        })
        .map_err(|error| error.at_stage(MeStage::Integration))?
        .map_err(|error| error.at_stage(MeStage::Integration))?;
        self.cache_derivative(time, &self.states, derivatives);
        Ok(())
    }

    pub(crate) fn event_indicators_into(&self, indicators: &mut [f64]) -> Result<(), MeError> {
        let indicator_count = self.event_indicator_sources.len();
        if indicators.len() != indicator_count {
            return Err(contract(format!(
                "event-indicator buffer has {} entries for {} indicators",
                indicators.len(),
                indicator_count,
            )));
        }
        self.require_active_lifecycle("get_event_indicators")?;
        if indicator_count == 0 {
            return Ok(());
        }
        let time = self.continuous_eval_time();
        if self.copy_cached_root_conditions_into(time, &self.states, indicators) {
            return Ok(());
        }
        let mut settled_guess = self.cached_continuous_solver_y(time, &self.states, &self.params);
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.evaluate_inventory_indicators(time, params, &mut settled_guess, indicators)
        })
        .map_err(|error| error.at_stage(MeStage::Integration))?
        .map_err(|error| error.at_stage(MeStage::Integration))?;
        self.orient_inventory_root_zeros(indicators);
        self.restore_frozen_zero_domains(indicators);
        self.cache_root_conditions(time, &self.states, indicators);
        Ok(())
    }

    fn evaluate_inventory_indicators(
        &self,
        time: f64,
        params: &[f64],
        settled_guess: &mut Option<Vec<f64>>,
        indicators: &mut [f64],
    ) -> Result<(), MeError> {
        if settled_guess.is_none() {
            *settled_guess = Some(self.runtime.full_solver_y(
                time,
                &self.states,
                params,
                ALGEBRAIC_REFRESH_TOL,
                UPDATE_MAX_ITERS,
            )?);
        }
        let needs_full_roots = self.event_indicator_sources.iter().any(|source| {
            matches!(
                source,
                solve::fmi::FmiEventIndicatorSource::RootCondition { .. }
                    | solve::fmi::FmiEventIndicatorSource::DelayDiscontinuity { .. }
            )
        });
        let mut full_roots = vec![0.0; self.runtime.root_condition_count()];
        if needs_full_roots {
            self.evaluate_root_conditions(time, params, settled_guess, &mut full_roots)?;
        }
        let needs_dynamic_deadlines = self.event_indicator_sources.iter().any(|source| {
            matches!(
                source,
                solve::fmi::FmiEventIndicatorSource::DynamicTimeEvent { .. }
            )
        });
        let dynamic_deadlines = if needs_dynamic_deadlines {
            self.runtime.eval_dynamic_time_event_rows(
                time,
                settled_guess.as_deref().unwrap_or(&[]),
                params,
            )?
        } else {
            Vec::new()
        };
        let model_root_count = self
            .runtime
            .model
            .problem
            .events
            .root_conditions
            .output_count();
        for (indicator, source) in indicators.iter_mut().zip(&self.event_indicator_sources) {
            *indicator = match *source {
                solve::fmi::FmiEventIndicatorSource::RootCondition { index } => full_roots
                    .get(index)
                    .copied()
                    .ok_or_else(|| contract("FMI root indicator source is out of range"))?,
                solve::fmi::FmiEventIndicatorSource::DynamicTimeEvent { index } => {
                    dynamic_deadlines
                        .get(index)
                        .copied()
                        .ok_or_else(|| contract("FMI dynamic-time indicator source is out of range"))?
                        - time
                }
                solve::fmi::FmiEventIndicatorSource::DelayDiscontinuity { index } => full_roots
                    .get(model_root_count + index)
                    .copied()
                    .ok_or_else(|| contract("FMI delay indicator source is out of range"))?,
            };
        }
        Ok(())
    }

    fn orient_inventory_root_zeros(&self, indicators: &mut [f64]) {
        for (indicator, source) in indicators.iter_mut().zip(&self.event_indicator_sources) {
            if *indicator != 0.0 {
                continue;
            }
            let solve::fmi::FmiEventIndicatorSource::RootCondition { index } = *source else {
                continue;
            };
            *indicator = match self
                .runtime
                .model
                .problem
                .events
                .root_zero_domains
                .get(index)
            {
                Some(solve::RootZeroDomain::Positive) => f64::EPSILON,
                Some(solve::RootZeroDomain::NonPositive) => -f64::EPSILON,
                Some(solve::RootZeroDomain::Previous) | None => 0.0,
            };
        }
    }

    fn evaluate_root_conditions(
        &self,
        time: f64,
        params: &[f64],
        settled_guess: &mut Option<Vec<f64>>,
        indicators: &mut [f64],
    ) -> Result<(), MeError> {
        // Solve retains relation roots that are useful during initialization
        // and event iteration but are not continuously monitored FMI
        // indicators. The checked root plan keeps the complete positional
        // vector while neutralizing those non-search rows, so the common host
        // cannot turn a parameter-static or purely algebraic relation into a
        // spurious state event.
        match settled_guess {
            Some(guess)
                if self
                    .runtime
                    .derivative_settled_coordinate_can_refresh_roots() =>
            {
                self.runtime
                    .eval_root_search_conditions_after_derivative_settle_into(
                        time,
                        params,
                        guess,
                        ALGEBRAIC_REFRESH_TOL,
                        UPDATE_MAX_ITERS,
                        indicators,
                    )
                    .map_err(MeError::from)?;
                Ok(())
            }
            Some(guess) => {
                self.runtime
                    .eval_root_search_conditions_with_guess_into(
                        time,
                        &self.states,
                        params,
                        guess,
                        ALGEBRAIC_REFRESH_TOL,
                        UPDATE_MAX_ITERS,
                        indicators,
                    )
                    .map_err(MeError::from)?;
                Ok(())
            }
            None => {
                self.runtime
                    .eval_root_search_conditions_into(
                        time,
                        &self.states,
                        params,
                        ALGEBRAIC_REFRESH_TOL,
                        UPDATE_MAX_ITERS,
                        indicators,
                    )
                    .map_err(MeError::from)?;
                Ok(())
            }
        }
    }

    fn restore_frozen_zero_domains(&self, indicators: &mut [f64]) {
        for (index, (indicator, source)) in indicators
            .iter_mut()
            .zip(&self.event_indicator_sources)
            .enumerate()
        {
            let previous_domain = match *source {
                solve::fmi::FmiEventIndicatorSource::RootCondition { index } => self
                    .runtime
                    .model
                    .problem
                    .events
                    .root_zero_domains
                    .get(index)
                    .is_none_or(|domain| matches!(domain, solve::RootZeroDomain::Previous)),
                solve::fmi::FmiEventIndicatorSource::DynamicTimeEvent { .. }
                | solve::fmi::FmiEventIndicatorSource::DelayDiscontinuity { .. } => true,
            };
            if *indicator != 0.0 || !previous_domain {
                continue;
            }
            let was_positive = self
                .frozen_indicator_positive
                .get(index)
                .copied()
                .unwrap_or(false);
            *indicator = if was_positive {
                f64::EPSILON
            } else {
                -f64::EPSILON
            };
        }
    }

    /// Freeze the standard indicator domains at one completed point and retain
    /// any domain changes for the next argument-free Event Mode transition.
    ///
    /// This is component-owned state: the host classifies roots for location,
    /// while the component independently observes the standard callback at its
    /// own accepted coordinate. No crossing vector crosses the FMI boundary.
    pub(super) fn complete_indicator_domains(&mut self) -> Result<bool, MeError> {
        let count = self.event_indicator_sources.len();
        if count == 0 {
            self.frozen_indicator_positive.clear();
            self.pending_root_crossings.clear();
            return Ok(false);
        }
        let mut indicators = Vec::new();
        indicators
            .try_reserve_exact(count)
            .map_err(|_| MeError::Allocation {
                context: "completed event-indicator domains",
                entries: count,
            })?;
        indicators.resize(count, 0.0);
        self.event_indicators_into(&mut indicators)?;
        let current = indicators
            .iter()
            .map(|indicator| *indicator > 0.0)
            .collect::<Vec<_>>();
        if self.frozen_indicator_positive.len() != count {
            self.frozen_indicator_positive = current;
            self.pending_root_crossings.clear();
            return Ok(false);
        }

        let changed = self
            .frozen_indicator_positive
            .iter()
            .zip(&current)
            .enumerate()
            .filter_map(|(index, (before, after))| (before != after).then_some((index, *after)))
            .collect::<Vec<_>>();
        let model_root_count = self
            .runtime
            .model
            .problem
            .events
            .root_conditions
            .output_count();
        let crossings = changed
            .iter()
            .filter_map(|(position, after)| {
                let index = match self.event_indicator_sources[*position] {
                    solve::fmi::FmiEventIndicatorSource::RootCondition { index } => index,
                    solve::fmi::FmiEventIndicatorSource::DelayDiscontinuity { index } => {
                        model_root_count + index
                    }
                    solve::fmi::FmiEventIndicatorSource::DynamicTimeEvent { .. } => return None,
                };
                Some(RootCrossing {
                    index,
                    post_relation_memory_value: if *after { 0.0 } else { 1.0 },
                })
            })
            .collect::<Vec<_>>();
        if !changed.is_empty() {
            self.capture_event_entry()?;
        }
        self.pending_root_crossings = crossings;
        self.frozen_indicator_positive = current;
        Ok(!changed.is_empty())
    }

    /// Seed the domain cache after Event Mode from the settled component state.
    ///
    /// A typed relation-memory target decides an exact-zero `Previous` root;
    /// roots without such a target retain the side the completed-step callback
    /// froze. Static strict/non-strict roots were already oriented by their
    /// checked `RootZeroDomain`.
    pub(super) fn seed_settled_indicator_domains(&mut self) -> Result<(), MeError> {
        let count = self.event_indicator_sources.len();
        if count == 0 {
            self.frozen_indicator_positive.clear();
            return Ok(());
        }
        let previous = self.frozen_indicator_positive.clone();
        let mut indicators = Vec::new();
        indicators
            .try_reserve_exact(count)
            .map_err(|_| MeError::Allocation {
                context: "settled event-indicator domains",
                entries: count,
            })?;
        indicators.resize(count, 0.0);
        self.event_indicators_into(&mut indicators)?;
        let targets = &self
            .runtime
            .model
            .problem
            .events
            .root_relation_memory_targets;
        let settled = indicators
            .iter()
            .enumerate()
            .map(|(index, indicator)| {
                let root_index = match self.event_indicator_sources[index] {
                    solve::fmi::FmiEventIndicatorSource::RootCondition { index } => Some(index),
                    solve::fmi::FmiEventIndicatorSource::DynamicTimeEvent { .. }
                    | solve::fmi::FmiEventIndicatorSource::DelayDiscontinuity { .. } => None,
                };
                match root_index.and_then(|index| targets.get(index).copied().flatten()) {
                    Some(ScalarSlot::P {
                        index: parameter, ..
                    }) => self
                        .params
                        .get(parameter)
                        .is_none_or(|memory| *memory <= 0.5),
                    _ => previous.get(index).copied().unwrap_or(*indicator > 0.0),
                }
            })
            .collect();
        self.frozen_indicator_positive = settled;
        Ok(())
    }

    /// Capture the component's own pre-event values before relation-memory
    /// overrides are consumed by Event Mode.
    pub(super) fn capture_event_entry(&mut self) -> Result<(), MeError> {
        let pre_y = self.runtime.full_solver_y(
            self.time,
            &self.states,
            &self.params,
            ALGEBRAIC_REFRESH_TOL,
            UPDATE_MAX_ITERS,
        )?;
        self.pending_event_pre_y = Some(pre_y);
        self.pending_event_pre_p = Some(self.params.clone());
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn verification_observable_state(&self) -> (MeState, u64, Vec<u64>, Vec<u64>) {
        (
            self.lifecycle.state(),
            self.time.to_bits(),
            self.states.iter().map(|value| value.to_bits()).collect(),
            self.params.iter().map(|value| value.to_bits()).collect(),
        )
    }

    #[cfg(test)]
    pub(crate) fn verification_canonicalize_committed_event_view(
        &mut self,
        event_time: f64,
        solver_y: &mut [f64],
    ) -> Result<(), MeError> {
        self.canonicalize_committed_event_view(event_time, solver_y, &[])
    }

    #[cfg(test)]
    pub(crate) fn verification_continuous_linearization_cache_matches(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) -> bool {
        self.continuous_linearization_cache_matches(time, state, parameters)
    }

    #[cfg(test)]
    pub(crate) fn verification_cache_continuous_linearization(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        solver_y: &[f64],
    ) {
        self.solver_y_guess.borrow_mut().clone_from_slice(solver_y);
        self.cache_continuous_linearization(time, state, parameters, solver_y);
    }

    #[cfg(test)]
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
            && root_crossings_bit_eq(&self.pending_root_crossings, &state.pending_root_crossings)
            && option_float_vec_bit_eq(&self.pending_event_pre_y, &state.pending_event_pre_y)
            && option_float_vec_bit_eq(&self.pending_event_pre_p, &state.pending_event_pre_p)
            && option_float_vec_bit_eq(&self.boundary_event_pre_y, &state.boundary_event_pre_y)
            && option_float_vec_bit_eq(&self.boundary_event_pre_p, &state.boundary_event_pre_p)
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
            && continuous_linearization_cache_bit_eq(
                self.continuous_linearization_cache.borrow().as_ref(),
                state.continuous_linearization_cache.as_ref(),
            )
            && observations_bit_eq(&self.initial_observations, &state.initial_observations)
            && option_float_bit_eq(self.max_step_duration, state.max_step_duration)
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
        Self::instantiate_with_execution_backend(source, config, None)
    }

    /// Instantiate with a host-supplied compiled-code execution backend.
    ///
    /// The backend arrives as the opaque [`crate::fmi_me::MeExecutionBackend`]
    /// handle so an integrator host never names a runtime object (SPEC_0038
    /// §Internal Solver Boundary); it is unwrapped here, inside the contract.
    pub fn instantiate_with_execution_backend(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
        execution_backend: Option<crate::fmi_me::MeExecutionBackend>,
    ) -> Result<Self, MeError> {
        let execution_backend =
            execution_backend.map(crate::fmi_me::MeExecutionBackend::into_runtime_backend);
        Self::instantiate_inner(source, config, execution_backend)
            .map_err(|failure| failure.at_stage(MeStage::Instantiate))
    }

    pub(super) fn instantiate_inner(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
        execution_backend: Option<Rc<dyn crate::SolveExecutionBackend>>,
    ) -> Result<Self, MeError> {
        let (model, event_indicator_sources, max_step_duration_value_reference, configuration) =
            source
                .into_parts()
                .map_err(|error| contract(error.to_string()))?;
        let delay_bearing = !model.problem.events.delays.delay_time_rhs.is_empty();
        if max_step_duration_value_reference.is_some() != delay_bearing {
            return Err(contract(if delay_bearing {
                "a delay-bearing component has no maximum-step-duration Float64 variable"
            } else {
                "a delay-free component declares a maximum-step-duration Float64 variable"
            }));
        }
        rumoca_eval_solve::reset_solve_row_eval_trace();
        validate_explicit_solve_model(model)?;
        let model = model
            .resolved_periodic_schedules_at(config.start_time)
            .map_err(|error| {
                contract(format!(
                    "periodic schedule cannot be anchored at FMI startTime: {error}"
                ))
            })?;
        let runtime = Rc::new(SolveRuntime::new_with_execution_backend(
            &model,
            execution_backend,
        )?);
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
            event_indicator_sources,
            instance_brand: Rc::new(()),
            instance_name: config.instance_name,
            lifecycle: MeLifecycle::instantiated(configuration),
            tolerance: config.tolerance,
            stop_time: config.stop_time,
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
            pending_root_crossings: Vec::new(),
            frozen_indicator_positive: Vec::new(),
            pending_event_pre_y: None,
            pending_event_pre_p: None,
            boundary_event_pre_y: None,
            boundary_event_pre_p: None,
            derivative_cache: RefCell::new(None),
            root_cache: RefCell::new(None),
            continuous_linearization_cache: RefCell::new(None),
            initial_observations: Vec::new(),
            max_step_duration: None,
            max_step_duration_value_reference,
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
        AlgebraicSettle {
            tol: ALGEBRAIC_REFRESH_TOL,
            max_iters: UPDATE_MAX_ITERS,
        }
    }

    pub(super) fn algebraic_projection_policy(&self) -> MeAlgebraicProjectionPolicy {
        MeAlgebraicProjectionPolicy {
            tolerance: self.tolerance,
            settle: self.numerics_settle(),
        }
    }

    pub(super) fn initialization_solver_y(&self) -> Result<Vec<f64>, MeError> {
        self.current_solver_y()
    }

    pub(super) fn with_callback_solver_y<R>(&self, f: impl FnOnce(&mut Vec<f64>) -> R) -> R {
        self.invalidate_continuous_linearization();
        f(&mut self.solver_y_guess.borrow_mut())
    }

    pub(super) fn directional_derivative_at_parameters(
        &self,
        time: f64,
        parameters: &[f64],
        settle: AlgebraicSettle,
        seed: &[f64],
        sensitivity: &mut [f64],
    ) -> Result<(), MeError> {
        let lin = AlgebraicLinearization {
            t: time,
            params: parameters,
            settle,
        };
        {
            let cache = self.continuous_linearization_cache.borrow();
            if cache
                .as_ref()
                .filter(|cached| cached.matches(time, &self.states, parameters))
                .is_some()
            {
                let solver_y = self.solver_y_guess.borrow();
                return self
                    .runtime
                    .eval_state_jacobian_v_at_settled_solver_y_into(
                        lin,
                        &solver_y,
                        seed,
                        sensitivity,
                    )
                    .map_err(MeError::from);
            }
        }
        self.with_callback_solver_y(|guess| {
            let result = self
                .runtime
                .eval_state_jacobian_v_ad_with_guess_into(
                    lin,
                    &self.states,
                    seed,
                    guess,
                    sensitivity,
                )
                .map_err(MeError::from);
            if result.is_ok() {
                self.cache_continuous_linearization(time, &self.states, parameters, guess);
            } else {
                self.invalidate_continuous_linearization();
            }
            result
        })
    }

    fn state_derivatives_at_parameters(
        &self,
        time: f64,
        parameters: &[f64],
        settle: AlgebraicSettle,
        derivatives: &mut [f64],
    ) -> Result<(), MeError> {
        self.with_callback_solver_y(|guess| {
            let result = self
                .runtime
                .eval_state_derivatives_with_guess_into(
                    time,
                    &self.states,
                    parameters,
                    guess,
                    settle.tol,
                    settle.max_iters,
                    derivatives,
                )
                .map_err(MeError::from);
            if result.is_ok() {
                self.cache_continuous_linearization(time, &self.states, parameters, guess);
            } else {
                self.invalidate_continuous_linearization();
            }
            result
        })
    }

    // -- internal solver vector ------------------------------------------

    pub(super) fn current_solver_y(&self) -> Result<Vec<f64>, MeError> {
        self.solver_y_at_time(self.public_time_eval_time(self.time))
    }

    /// Build one atomic public observation outside Event Mode.
    ///
    /// Periodic activation lanes are true only while Event Mode consumes a
    /// tick; MLS event indicators are false on either public side. Rewriting
    /// those hidden lanes and the construction-issued dependent discrete rows
    /// in cloned storage prevents an event-internal pulse from leaking into
    /// public output without mutating the component's canonical event state.
    pub(super) fn observation_coordinate(&self) -> Result<(Vec<f64>, Vec<f64>), MeError> {
        let time = self.public_time_eval_time(self.time);
        let parameters = self.params.clone();
        let mut solver_y = self.solver_y_guess.borrow().clone();
        if solver_y.len() < self.states.len() {
            return Err(contract(format!(
                "observation solver vector has {} entries for {} state values",
                solver_y.len(),
                self.states.len()
            )));
        }
        solver_y[..self.states.len()].copy_from_slice(&self.states);
        self.refresh_public_observation_coordinate(solver_y, parameters, time)
    }

    fn refresh_public_observation_coordinate(
        &self,
        mut solver_y: Vec<f64>,
        mut parameters: Vec<f64>,
        time: f64,
    ) -> Result<(Vec<f64>, Vec<f64>), MeError> {
        let settle = self.numerics_settle();
        let states = solver_y
            .get(..self.state_count)
            .ok_or_else(|| {
                contract(format!(
                    "observation solver vector has {} entries for {} state values",
                    solver_y.len(),
                    self.state_count
                ))
            })?
            .to_vec();
        if self.runtime.has_delay_channels() {
            self.runtime
                .refresh_delay_values(time, &solver_y, &mut parameters)
                .map_err(MeError::from)?;
        }
        write_observation_clock_activation_params(&self.runtime.model, &mut parameters);
        // Seed the coupled public fixed point from the already settled
        // component coordinate. Direct activation aliases therefore update
        // before the first algebraic projection, while a row that depends on
        // an activation-sensitive algebraic is corrected by the checked loop
        // below after that projection.
        self.runtime
            .refresh_observation_discrete_rows(
                &mut solver_y,
                &mut parameters,
                time,
                settle.tol,
                settle.max_iters,
            )
            .map_err(MeError::from)?;
        self.runtime
            .full_solver_y_with_guess(
                time,
                &states,
                &parameters,
                &mut solver_y,
                settle.tol,
                settle.max_iters,
            )
            .map_err(MeError::from)?;
        if !self
            .runtime
            .model
            .problem
            .discrete
            .observation_refresh_reads_y
        {
            return Ok((solver_y, parameters));
        }
        for _ in 0..settle.max_iters {
            let changed = self
                .runtime
                .refresh_observation_discrete_rows(
                    &mut solver_y,
                    &mut parameters,
                    time,
                    settle.tol,
                    settle.max_iters,
                )
                .map_err(MeError::from)?;
            if !changed {
                return Ok((solver_y, parameters));
            }
            self.runtime
                .full_solver_y_with_guess(
                    time,
                    &states,
                    &parameters,
                    &mut solver_y,
                    settle.tol,
                    settle.max_iters,
                )
                .map_err(MeError::from)?;
        }
        Err(MeError::from(
            crate::runtime::solve_ops::RuntimeSolveError::solve_ir(
                "public observation refresh did not converge",
            ),
        ))
    }

    pub(super) fn refresh_initial_observation(
        &self,
        observation: &InitialEventObservation,
    ) -> Result<MeObservation, MeError> {
        let (solver_y, parameters) = self.refresh_public_observation_coordinate(
            observation.y.clone(),
            observation.p.clone(),
            observation.t,
        )?;
        Ok(MeObservation {
            time: observation.t,
            solver_y,
            parameters,
            instance_brand: Rc::clone(&self.instance_brand),
        })
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
        self.max_step_duration =
            self.runtime
                .refresh_delay_values(self.time, &solver_y, &mut self.params)?;
        if !self.continuous_linearization_cache_matches(self.time, &self.states, &self.params) {
            self.invalidate_continuous_linearization();
            self.runtime.full_solver_y_with_guess(
                self.time,
                &self.states,
                &self.params,
                &mut solver_y,
                settle.tol,
                settle.max_iters,
            )?;
            self.cache_continuous_linearization(self.time, &self.states, &self.params, &solver_y);
        }
        self.runtime
            .commit_delay_history(self.time, &solver_y, &self.params)?;
        Ok(())
    }

    // -- caches ------------------------------------------------------------

    fn copy_cached_derivative_into(&self, time: f64, state: &[f64], out: &mut [f64]) -> bool {
        let cache = self.derivative_cache.borrow();
        let Some(cached) = cache.as_ref() else {
            return false;
        };
        if cached.time.to_bits() != time.to_bits()
            || !state_values_match(&cached.state, state)
            || cached.derivative.len() != out.len()
        {
            return false;
        }
        out.copy_from_slice(&cached.derivative);
        true
    }

    #[cfg(test)]
    pub(crate) fn cached_derivative(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        let cache = self.derivative_cache.borrow();
        let cached = cache.as_ref()?;
        (cached.time.to_bits() == time.to_bits() && state_values_match(&cached.state, state))
            .then(|| cached.derivative.clone())
    }

    pub(crate) fn cache_derivative(&self, time: f64, state: &[f64], derivative: &[f64]) {
        let mut cache = self.derivative_cache.borrow_mut();
        if let Some(cached) = cache.as_mut() {
            cached.time = time;
            cached.state.clone_from_slice(state);
            cached.derivative.clone_from_slice(derivative);
        } else {
            *cache = Some(CachedDerivative {
                time,
                state: state.to_vec(),
                derivative: derivative.to_vec(),
            });
        }
    }

    pub(super) fn clear_derivative_cache(&self) {
        *self.derivative_cache.borrow_mut() = None;
        *self.continuous_linearization_cache.borrow_mut() = None;
    }

    pub(super) fn clear_runtime_caches(&self) {
        self.clear_callback_value_caches();
        *self.continuous_linearization_cache.borrow_mut() = None;
    }

    pub(super) fn clear_callback_value_caches(&self) {
        *self.derivative_cache.borrow_mut() = None;
        *self.root_cache.borrow_mut() = None;
    }

    fn copy_cached_root_conditions_into(&self, time: f64, state: &[f64], out: &mut [f64]) -> bool {
        let cache = self.root_cache.borrow();
        let Some(cached) = cache.as_ref() else {
            return false;
        };
        if cached.time.to_bits() != time.to_bits()
            || !state_values_match(&cached.state, state)
            || cached.values.len() != out.len()
        {
            return false;
        }
        out.copy_from_slice(&cached.values);
        true
    }

    #[cfg(test)]
    pub(crate) fn cached_root_conditions(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        let cache = self.root_cache.borrow();
        let cached = cache.as_ref()?;
        (cached.time.to_bits() == time.to_bits() && state_values_match(&cached.state, state))
            .then(|| cached.values.clone())
    }

    pub(crate) fn cache_root_conditions(&self, time: f64, state: &[f64], values: &[f64]) {
        let mut cache = self.root_cache.borrow_mut();
        if let Some(cached) = cache.as_mut() {
            cached.time = time;
            cached.state.clone_from_slice(state);
            cached.values.clone_from_slice(values);
        } else {
            *cache = Some(CachedRootConditions {
                time,
                state: state.to_vec(),
                values: values.to_vec(),
            });
        }
    }

    pub(super) fn cache_continuous_linearization(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        _solver_y: &[f64],
    ) {
        let mut cache = self.continuous_linearization_cache.borrow_mut();
        if let Some(cached) = cache.as_mut() {
            cached.time = time;
            cached.state.clone_from_slice(state);
            cached.parameters.clone_from_slice(parameters);
        } else {
            *cache = Some(CachedContinuousLinearization {
                time,
                state: state.to_vec(),
                parameters: parameters.to_vec(),
            });
        }
    }

    fn invalidate_continuous_linearization(&self) {
        *self.continuous_linearization_cache.borrow_mut() = None;
    }

    fn continuous_linearization_cache_matches(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) -> bool {
        self.continuous_linearization_cache
            .borrow()
            .as_ref()
            .is_some_and(|cached| cached.matches(time, state, parameters))
    }

    fn cached_continuous_solver_y(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) -> Option<Vec<f64>> {
        self.continuous_linearization_cache
            .borrow()
            .as_ref()
            .filter(|cached| cached.matches(time, state, parameters))
            .map(|_| self.solver_y_guess.borrow().clone())
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
        self.copy_states_from_solver_y(&solver_y);
        self.invalidate_continuous_linearization();
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

    /// [`SolveMeKernel::project_continuous_states`], unannotated; the
    /// trait method attaches [`MeStage::ManifoldProjection`].
    pub(super) fn project_continuous_states_inner(
        &mut self,
        states: &mut [f64],
    ) -> Result<bool, MeError> {
        let before = states.to_vec();
        let projected = self.project_continuous_states_for_observation(states)?;
        let changed = projected && runtime_values_changed(&before, states, self.tolerance);
        if !changed {
            // A correction below the component's certified runtime tolerance
            // is the same accepted point. Keeping the native endpoint avoids
            // discarding multistep history for roundoff-sized projections.
            states.copy_from_slice(&before);
        }
        self.last_projection_changed = changed;
        Ok(changed)
    }

    /// Project an off-point observation without making its correction the
    /// accepted-step fact consumed by `completed_integrator_step`.
    pub(crate) fn project_continuous_states_for_observation(
        &self,
        states: &mut [f64],
    ) -> Result<bool, MeError> {
        if !self.runtime.requires_state_manifold_projection() {
            return Ok(false);
        }
        let time = self.time;
        let settle = self.numerics_settle();
        let mut solver_y = self.solver_y_guess.borrow().clone();
        self.runtime.full_solver_y_with_guess(
            time,
            states,
            &self.params,
            &mut solver_y,
            settle.tol,
            settle.max_iters,
        )?;
        let changed = self.runtime.project_state_manifold(
            &mut solver_y,
            &self.params,
            time,
            ALGEBRAIC_REFRESH_TOL,
        )?;
        states.copy_from_slice(&solver_y[..self.state_count]);
        solver_y[..self.state_count].copy_from_slice(states);
        self.invalidate_continuous_linearization();
        *self.solver_y_guess.borrow_mut() = solver_y;
        Ok(changed)
    }

    /// Query the next component-owned scheduled event while constructing
    /// `fmi3UpdateDiscreteStates` output.
    pub(super) fn next_event_stop_inner(&mut self, horizon: f64) -> Result<MeEventStop, MeError> {
        let solver_y = self
            .runtime
            .dynamic_time_event_stop_reads_solver_y()
            .then(|| self.current_solver_y())
            .transpose()?;
        let (time, event) = self.runtime.next_runtime_event_stop(
            solver_y.as_deref().unwrap_or(&[]),
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

    // -- event boundary ----------------------------------------------------

    pub(super) fn apply_discrete_event_updates(
        &mut self,
        event_time: f64,
        _event: RuntimeEventStop,
        row_filter: EventUpdateRowFilter,
        iteration_y: Option<Vec<f64>>,
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
        let mut solver_y = iteration_y
            .map(Ok)
            .unwrap_or_else(|| self.event_iteration_solver_y(&event_entry_y))?;
        let pending_root_overrides = self.take_pending_event_root_overrides();
        let root_overrides = pending_root_overrides.as_slice();
        let runtime = Rc::clone(&self.runtime);
        let projection_runtime = Rc::clone(&runtime);
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
            move |y, p| project_event_algebraics(&projection_runtime, y, p, event_time, policy),
        )?;
        // Unrelated algebraic/output lanes remain lazy in the retained solver
        // seed. Their owning callback refresh plan materializes them if and
        // when a derivative, root, or visible-value consumer asks for them.
        self.commit_event_runtime_state(event_time, solver_y, root_overrides)?;
        self.record_event_action_outcome(outcome, event_time)?;
        // `commit_event_runtime_state` leaves a checked post-event
        // linearization in the retained solver vector. Event actions do not
        // mutate model storage, so only callback-result caches are stale here;
        // discarding the linearization would force Continuous-Time Mode to
        // solve the identical coordinate again.
        self.clear_callback_value_caches();
        Ok(())
    }

    fn take_pending_event_root_overrides(&mut self) -> Vec<(usize, f64)> {
        let pending = self.pending_root_crossings.drain(..).collect::<Vec<_>>();
        pending
            .iter()
            .map(|crossing| (crossing.index, crossing.post_relation_memory_value))
            .collect()
    }

    pub(super) fn commit_event_runtime_state(
        &mut self,
        event_time: f64,
        mut solver_y: Vec<f64>,
        root_overrides: &[(usize, f64)],
    ) -> Result<(), MeError> {
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
        self.copy_states_from_solver_y(&solver_y);
        self.invalidate_continuous_linearization();
        *self.solver_y_guess.borrow_mut() = solver_y;
        if history_changed {
            let solver_y = self.solver_y_guess.borrow();
            self.cache_continuous_linearization(event_time, &self.states, &self.params, &solver_y);
        }
        self.commit_delay_point()?;
        Ok(())
    }

    pub(super) fn event_iteration_solver_y(
        &self,
        event_entry_y: &[f64],
    ) -> Result<Vec<f64>, MeError> {
        Ok(event_entry_y.to_vec())
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
        for _ in 0..policy.settle.max_iters {
            let mut changed = runtime.apply_post_commit_assignments_until_stable(
                solver_y,
                &mut self.params,
                event_time,
                policy.settle.tol,
                policy.settle.max_iters,
            )?;
            changed |=
                project_algebraics(&runtime, solver_y, &mut self.params, event_time, policy)?;
            changed |= runtime.update_algebraic_relation_memory_from_solver_y_except_overrides(
                event_time,
                solver_y,
                &mut self.params,
                root_relation_overrides,
            )?;
            if !changed {
                return Ok(());
            }
        }
        Err(contract(format!(
            "post-commit derived event view did not converge at t={event_time}"
        )))
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
                // A scheduled time event has no numerical root bracket. Use
                // the adjacent representable coordinate for its semantic
                // left limit: discontinuous time expressions remain on their
                // pre-event branch, while continuous expressions differ from
                // the exact tick by at most one ULP. A located state root
                // instead uses the importer's tolerance-clearing left probe.
                if self.advance_state_to_event_right_limit {
                    timeline::event_left_limit_time(event_time)
                } else {
                    timeline::event_left_probe_time(event_time, self.tolerance)
                }
            }
            EventPreMode::FollowCurrent => self.public_time_eval_time(self.time),
        };
        // Every lane of the entry snapshot belongs to one fresh evaluation.
        // In both cases `solver_y_at_time` evaluates against the exact
        // importer-owned event-entry state; only the evaluation coordinate
        // differs between a scheduled left limit and a located root bracket.
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
            },
            move |y, p, t| project_algebraics(&projection_runtime, y, p, t, policy),
        )?;
        self.copy_states_from_solver_y(&solver_y);
        self.invalidate_continuous_linearization();
        *self.solver_y_guess.borrow_mut() = solver_y;
        self.time = outcome.final_t;
        self.initial_observations = outcome
            .observations
            .iter()
            .map(|observation| self.refresh_initial_observation(observation))
            .collect::<Result<Vec<_>, _>>()?;
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
                let scheduled_event =
                    scheduled.map(|coincidence| (coincidence.event.time, coincidence.event.event));
                let pending_event = self
                    .pending_event_stop
                    .filter(|(time, _)| time_match_with_tol(*time, entry.event_time));
                let coincident_time_event =
                    merge_coincident_event_stops(scheduled_event, pending_event);
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
                        event_boundary_horizon(event, entry.horizon, self.stop_time)
                    });
                let outcome =
                    self.process_runtime_event_boundary(event_time, horizon_t, tolerance, event)?;
                let right_limit_t = outcome.right_limit_t;
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
                let outcome = self.process_runtime_event_boundary(
                    entry.event_time,
                    event_boundary_horizon(event, entry.horizon, self.stop_time),
                    tolerance,
                    event,
                )?;
                self.advance_state_to_event_right_limit = false;
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

fn merge_coincident_event_stops(
    scheduled: Option<(f64, RuntimeEventStop)>,
    pending: Option<(f64, RuntimeEventStop)>,
) -> Option<(f64, RuntimeEventStop)> {
    match (scheduled, pending) {
        (Some((scheduled_time, scheduled_event)), Some((pending_time, pending_event))) => Some((
            scheduled_time.max(pending_time),
            scheduled_event.merge(pending_event),
        )),
        (Some(event), None) | (None, Some(event)) => Some(event),
        (None, None) => None,
    }
}

#[cfg(test)]
mod tests;
