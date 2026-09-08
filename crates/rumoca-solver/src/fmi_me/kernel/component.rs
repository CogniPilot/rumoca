pub(super) mod event_storage;
mod instantiation;
#[cfg(test)]
mod storage_identity;

use super::event_boundary::event_boundary_horizon;
use super::indicator_plan::{IndicatorReading, IndicatorZeroSide};
use super::*;
use event_storage::EventVectorLatch;
#[cfg(test)]
use storage_identity::{
    DerivativeScratchIdentity, EventStageStorageIdentity, IndicatorStorageIdentity,
};

impl SolveMeKernel {
    /// `fmi3InstantiateModelExchange`: project the checked kernel once.
    pub fn instantiate(source: MeModelSource, config: &MeInstanceConfig) -> Result<Self, MeError> {
        Self::instantiate_with_execution(
            source,
            config,
            crate::fmi_me::MeExecutionSelection::Interpreter,
        )
    }

    pub fn instantiate_with_execution(
        source: MeModelSource,
        config: &MeInstanceConfig,
        execution: crate::fmi_me::MeExecutionSelection,
    ) -> Result<Self, MeError> {
        let (lifecycle, body) = MeKernelBody::instantiate_body(source, config, execution)
            .map_err(|failure| failure.at_stage(MeStage::Instantiate))?;
        let event_stage = Box::new(
            body.construction_event_stage()
                .map_err(|failure| failure.at_stage(MeStage::Instantiate))?,
        );
        let event_runtime_checkpoint = body.runtime.snapshot();
        Ok(Self {
            lifecycle,
            body,
            event_stage,
            event_runtime_checkpoint,
        })
    }

    #[cfg(test)]
    pub(crate) fn verification_indicator_storage(&self) -> IndicatorStorageIdentity {
        self.body.verification_indicator_storage()
    }

    #[cfg(test)]
    pub(crate) fn verification_indicator_domains(&self) -> (Vec<bool>, Vec<bool>) {
        self.body.indicator_storage.domain_values()
    }

    #[cfg(test)]
    pub(crate) fn verification_derivative_scratch_identity(&self) -> DerivativeScratchIdentity {
        self.body.verification_derivative_scratch_identity()
    }

    #[cfg(test)]
    pub(crate) fn verification_event_stage_identity(&self) -> EventStageStorageIdentity {
        EventStageStorageIdentity {
            live_model: std::ptr::from_ref(self.body.runtime.model()) as usize,
            stage_model: std::ptr::from_ref(self.event_stage.runtime.model()) as usize,
            live_linked_facts: std::ptr::from_ref(self.body.runtime.fmi_linked_runtime_facts())
                as usize,
            stage_linked_facts: std::ptr::from_ref(
                self.event_stage.runtime.fmi_linked_runtime_facts(),
            ) as usize,
            linked_descriptor_table: self
                .body
                .runtime
                .fmi_linked_runtime_facts()
                .float64_descriptors()
                .as_ptr() as usize,
            live_entry_table: self.body.indicator_plan().entries().as_ptr() as usize,
            stage_entry_table: self.event_stage.indicator_plan().entries().as_ptr() as usize,
            stage_body: std::ptr::from_ref(self.event_stage.as_ref()) as usize,
            live_indicator_buffers: self.body.indicator_storage.storage_identities(),
            stage_indicator_buffers: self.event_stage.indicator_storage.storage_identities(),
            live_event_buffers: self.body.event_buffer_identity(),
            stage_event_buffers: self.event_stage.event_buffer_identity(),
            live_transaction_buffers: self.body.transaction_buffer_identity(),
            stage_transaction_buffers: self.event_stage.transaction_buffer_identity(),
        }
    }

    #[cfg(test)]
    pub(crate) fn verification_observable_state(&self) -> (MeState, u64, Vec<u64>, Vec<u64>) {
        let (time, states, params) = self.body.verification_observable_body();
        (self.lifecycle.state(), time, states, params)
    }

    #[cfg(test)]
    pub(crate) fn verification_canonicalize_committed_event_view(
        &mut self,
        event_time: f64,
        solver_y: &mut [f64],
    ) -> Result<(), MeError> {
        self.body
            .verification_canonicalize_committed_event_view(event_time, solver_y)
    }

    #[cfg(test)]
    pub(crate) fn verification_continuous_linearization_cache_matches(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) -> bool {
        self.body
            .verification_continuous_linearization_cache_matches(time, state, parameters)
    }

    #[cfg(test)]
    pub(crate) fn verification_cache_continuous_linearization(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        solver_y: &[f64],
    ) {
        self.body
            .verification_cache_continuous_linearization(time, state, parameters, solver_y);
    }

    #[cfg(test)]
    pub(crate) fn verification_matches_snapshot(&self, saved: &MeFmuState) -> bool {
        self.lifecycle.matches_saved(&saved.component.lifecycle)
            && self.body.verification_matches_snapshot(saved)
    }

    #[cfg(test)]
    pub(crate) fn cached_derivative(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        self.body.cached_derivative(time, state)
    }

    #[cfg(test)]
    pub(crate) fn cache_derivative(&self, time: f64, state: &[f64], derivative: &[f64]) {
        self.body.cache_derivative(time, state, derivative);
    }

    #[cfg(test)]
    pub(super) fn numerics_settle(&self) -> AlgebraicSettle {
        self.body.numerics_settle()
    }
}

impl MeKernelBody {
    pub(super) fn indicator_plan(&self) -> &FmiIndicatorPlan {
        self.runtime.fmi_linked_runtime_facts().indicator_plan()
    }

    pub(super) fn pending_event_entry(&self) -> PendingEventEntry {
        let scheduled_here = self
            .pending_event_stop
            .is_some_and(|(time, _)| time_match_with_tol(time, self.time));
        PendingEventEntry {
            cause: if self.pending_state_event_entry || !scheduled_here {
                PendingEventCause::State
            } else {
                PendingEventCause::Time
            },
            event_time: self.time,
            horizon: self.stop_time.max(self.time),
        }
    }

    pub(crate) fn continuous_state_derivatives_into(
        &self,
        derivatives: &mut [f64],
    ) -> Result<(), MeError> {
        if derivatives.len() != self.state_domain.len() {
            return Err(contract(format!(
                "continuous-state derivative buffer has {} entries for {} states",
                derivatives.len(),
                self.state_domain.len(),
            )));
        }
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

    pub(super) fn event_indicators_into(
        &self,
        indicators: &mut FmiPublicationIndicatorValues,
    ) -> Result<(), MeError> {
        if indicators.is_empty() {
            return Ok(());
        }
        let time = self.continuous_eval_time();
        if self
            .indicator_storage
            .copy_cache_into_publication(time, &self.states, indicators)
        {
            return Ok(());
        }
        let mut working = self.indicator_storage.working_values_mut();
        self.evaluate_current_indicators(time, &mut working)?;
        self.indicator_storage
            .copy_working_to_publication(&working, indicators);
        self.indicator_storage
            .store_publication_cache(time, &self.states, indicators);
        Ok(())
    }

    fn refresh_working_indicators(&self) -> Result<(), MeError> {
        let time = self.continuous_eval_time();
        let mut working = self.indicator_storage.working_values_mut();
        if self
            .indicator_storage
            .copy_cache_into_working(time, &self.states, &mut working)
        {
            return Ok(());
        }
        self.evaluate_current_indicators(time, &mut working)
    }

    fn evaluate_current_indicators(
        &self,
        time: f64,
        indicators: &mut WorkingPublishedIndicatorValues,
    ) -> Result<(), MeError> {
        let mut settled_guess = self.cached_continuous_solver_y(time, &self.states, &self.params);
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.evaluate_inventory_indicators(time, params, &mut settled_guess, indicators)
        })
        .map_err(|error| error.at_stage(MeStage::Integration))?
        .map_err(|error| error.at_stage(MeStage::Integration))?;
        self.apply_indicator_zero_sides(indicators);
        Ok(())
    }

    /// Read every FMI indicator position through the resolved plan.
    ///
    /// The plan already decides which runtime vectors this model reads, so the
    /// only per-read cost is the evaluation each declared position needs. A
    /// dynamic-time deadline is the one source that needs a settled algebraic
    /// coordinate of its own; a root-only inventory keeps the root search's
    /// own restricted refresh.
    fn evaluate_inventory_indicators(
        &self,
        time: f64,
        params: &[f64],
        settled_guess: &mut Option<Vec<f64>>,
        indicators: &mut WorkingPublishedIndicatorValues,
    ) -> Result<(), MeError> {
        let mut root_values = self.indicator_storage.root_values_mut();
        let mut deadlines = self.indicator_storage.deadline_values_mut();
        if self.indicator_plan().reads_deadlines() && settled_guess.is_none() {
            *settled_guess = Some(self.runtime.full_solver_y(
                time,
                &self.states,
                params,
                ALGEBRAIC_REFRESH_TOL,
                UPDATE_MAX_ITERS,
            )?);
        }
        if self.indicator_plan().reads_root_values() {
            root_values.as_mut_slice().fill(0.0);
            self.evaluate_root_conditions(time, params, settled_guess, root_values.as_mut_slice())?;
        }
        if self.indicator_plan().reads_deadlines() {
            let guess = settled_guess.as_deref().ok_or_else(|| {
                contract("FMI dynamic-time indicators need a settled algebraic coordinate")
            })?;
            self.runtime.eval_dynamic_time_event_rows_into(
                time,
                guess,
                params,
                deadlines.as_mut_slice(),
            )?;
        }
        for (indicator, entry) in indicators
            .as_mut_slice()
            .iter_mut()
            .zip(self.indicator_plan().entries())
        {
            *indicator = indicator_reading_value(entry.reading(), time, &root_values, &deadlines);
        }
        Ok(())
    }

    /// Report an exact zero on the side the plan assigned that position.
    fn apply_indicator_zero_sides(&self, indicators: &mut WorkingPublishedIndicatorValues) {
        for (position, (indicator, entry)) in indicators
            .as_mut_slice()
            .iter_mut()
            .zip(self.indicator_plan().entries())
            .enumerate()
        {
            if *indicator != 0.0 {
                continue;
            }
            *indicator = match entry.zero_side() {
                IndicatorZeroSide::Positive => f64::EPSILON,
                IndicatorZeroSide::NonPositive => -f64::EPSILON,
                IndicatorZeroSide::Frozen => self.frozen_indicator_zero(position),
            };
        }
    }

    /// The side the previous completed point froze one indicator on.
    fn frozen_indicator_zero(&self, position: usize) -> f64 {
        let positive = self.indicator_storage.frozen_domains()[position];
        if positive {
            f64::EPSILON
        } else {
            -f64::EPSILON
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
                        AlgebraicSettle {
                            tol: ALGEBRAIC_REFRESH_TOL,
                            max_iters: UPDATE_MAX_ITERS,
                        },
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

    /// Retain the standard domains at one completed point without classifying
    /// state or time events. FMI assigns that classification to the importer.
    pub(super) fn freeze_completed_indicator_domains(&mut self) -> Result<(), MeError> {
        if self.indicator_plan().is_empty() {
            self.pending_root_crossings.clear();
            return Ok(());
        }
        self.refresh_working_indicators()?;
        self.indicator_storage.freeze_working_domains();
        self.pending_root_crossings.clear();
        Ok(())
    }

    /// Classify the component-private domain pair only after the importer has
    /// independently selected Event Mode.
    pub(super) fn classify_entered_state_event(&mut self) -> Result<(), MeError> {
        let domain_changed = self
            .indicator_storage
            .working_domains()
            .iter()
            .zip(self.indicator_storage.frozen_domains())
            .any(|(before, after)| before != after);
        if !domain_changed {
            self.pending_root_crossings.clear();
            return Ok(());
        }
        self.capture_event_entry()?;
        let mut crossings = std::mem::take(&mut self.pending_root_crossings);
        crossings.clear();
        crossings.extend(
            self.indicator_storage
                .working_domains()
                .iter()
                .zip(self.indicator_storage.frozen_domains())
                .enumerate()
                .filter(|(_, (before, after))| before != after)
                .filter_map(|(position, (_, after))| {
                    Some(RootCrossing {
                        index: self.indicator_plan().crossing_root_index(position)?,
                        post_relation_memory_value: if *after { 0.0 } else { 1.0 },
                    })
                }),
        );
        self.pending_root_crossings = crossings;
        self.indicator_storage.copy_frozen_to_working_domains();
        Ok(())
    }

    pub(super) fn cache_accepted_derivatives(&self) -> Result<(), MeError> {
        let mut accepted = self.accepted_derivative_scratch.borrow_mut();
        self.continuous_state_derivatives_into(&mut accepted)
    }

    /// Seed the domain cache after Event Mode from the settled component state.
    ///
    /// A typed relation-memory target decides an exact-zero `Previous` root;
    /// roots without such a target retain the side the completed-step callback
    /// froze. Static strict/non-strict roots were already oriented by their
    /// checked `RootZeroDomain`.
    pub(super) fn seed_settled_indicator_domains(&mut self) -> Result<(), MeError> {
        let count = self.indicator_plan().len();
        self.refresh_working_indicators()?;
        for position in 0..count {
            let settled = match self.indicator_plan().relation_memory_target(position) {
                Some(target) => constructed_relation_memory_domain(&self.params, position, target)?,
                None => self.indicator_storage.working_values().as_slice()[position] > 0.0,
            };
            self.indicator_storage.set_domain(position, settled);
        }
        Ok(())
    }

    /// Capture the component's own pre-event values before relation-memory
    /// overrides are consumed by Event Mode.
    pub(super) fn capture_event_entry(&mut self) -> Result<(), MeError> {
        self.runtime.full_solver_y_into(
            self.time,
            &self.states,
            &self.params,
            ALGEBRAIC_REFRESH_TOL,
            UPDATE_MAX_ITERS,
            self.pending_event_pre_y.storage_mut(),
        )?;
        self.pending_event_pre_y.mark_occupied();
        self.pending_event_pre_p
            .set(&self.params, "event-entry parameter latch")?;
        Ok(())
    }

    /// The identity of the resolved indicator table and of the storage every
    /// indicator read uses.
    ///
    /// The plan shape comes from the constructor and the buffer addresses are
    /// the ones reserved with it, so a step that rebuilt or regrew either would
    /// change this value.
    #[cfg(test)]
    pub(crate) fn verification_indicator_storage(&self) -> IndicatorStorageIdentity {
        IndicatorStorageIdentity {
            linked_facts: std::ptr::from_ref(self.runtime.fmi_linked_runtime_facts()) as usize,
            entry_table: self.indicator_plan().entries().as_ptr() as usize,
            entries: self
                .indicator_plan()
                .entries()
                .iter()
                .map(|entry| {
                    (
                        entry.reading(),
                        entry.zero_side(),
                        entry.crossing_root_index(),
                    )
                })
                .collect(),
            role_widths: self.indicator_storage.role_widths(),
            buffers: self.indicator_storage.storage_identities(),
        }
    }

    /// The identity of the construction-reserved caller-publication storage
    /// used by the derivative getters.
    ///
    /// For the nonzero verification fixture, the constructor-issued widths
    /// make replacement or growth of these publication buffers observable as
    /// a changed pointer or length. Evaluator, JVP, and delay workspace
    /// allocation is outside this identity.
    #[cfg(test)]
    pub(crate) fn verification_derivative_scratch_identity(&self) -> DerivativeScratchIdentity {
        let id = |cell: &RefCell<Vec<f64>>| {
            let buffer = cell.borrow();
            (buffer.as_ptr() as usize, buffer.len())
        };
        DerivativeScratchIdentity {
            output: id(&self.derivative_output_scratch),
            directional_seed: id(&self.directional_seed_scratch),
            directional_sensitivity: id(&self.directional_sensitivity_scratch),
            directional_serialized: id(&self.directional_serialized_scratch),
        }
    }

    #[cfg(test)]
    fn transaction_buffer_identity(&self) -> [usize; 16] {
        let derivative = self.derivative_cache.borrow();
        let indicator_cache = self.indicator_storage.storage_identities()[6].0;
        let linearization = self.continuous_linearization_cache.borrow();
        [
            self.states.as_ptr() as usize,
            self.params.as_ptr() as usize,
            self.pending_root_crossings.as_ptr() as usize,
            self.solver_y_guess.borrow().as_ptr() as usize,
            self.delay_params_scratch.borrow().as_ptr() as usize,
            self.delay_solver_y_scratch.borrow().as_ptr() as usize,
            self.accepted_derivative_scratch.borrow().as_ptr() as usize,
            self.derivative_output_scratch.borrow().as_ptr() as usize,
            self.directional_seed_scratch.borrow().as_ptr() as usize,
            self.directional_sensitivity_scratch.borrow().as_ptr() as usize,
            self.directional_serialized_scratch.borrow().as_ptr() as usize,
            derivative.state.as_ptr() as usize,
            derivative.derivative.as_ptr() as usize,
            indicator_cache,
            linearization.state.as_ptr() as usize,
            linearization.parameters.as_ptr() as usize,
        ]
    }

    #[cfg(test)]
    fn event_buffer_identity(&self) -> [usize; 9] {
        [
            self.pending_event_pre_y.storage_identity(),
            self.pending_event_pre_p.storage_identity(),
            self.boundary_event_pre_y.storage_identity(),
            self.boundary_event_pre_p.storage_identity(),
            self.settled_initialization_y.storage_identity(),
            self.event_solver_y_work.as_ptr() as usize,
            self.event_state_before.as_ptr() as usize,
            self.scheduled_root_index_scratch.as_ptr() as usize,
            self.root_override_scratch.as_ptr() as usize,
        ]
    }

    #[cfg(test)]
    fn verification_observable_body(&self) -> (u64, Vec<u64>, Vec<u64>) {
        (
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
        self.stop_time.to_bits() == state.stop_time.to_bits()
            && self.time.to_bits() == state.time.to_bits()
            && self.set_time_bounds.bit_eq(state.set_time_bounds)
            && option_float_bit_eq(self.post_event_eval_time, state.post_event_eval_time)
            && self.event_anchor_time.to_bits() == state.event_anchor_time.to_bits()
            && float_slice_bit_eq(&self.states, &state.states)
            && float_slice_bit_eq(&self.params, &state.params)
            && self.stop_schedule.bit_eq(&state.stop_schedule)
            && option_event_entry_bit_eq(self.pending_event_entry, state.pending_event_entry)
            && self.pending_state_event_entry == state.pending_state_event_entry
            && option_event_stop_bit_eq(self.pending_event_stop, state.pending_event_stop)
            && self.advance_state_to_event_right_limit == state.advance_state_to_event_right_limit
            && self.state_time_coincidence == state.state_time_coincidence
            && self.initial_event_pending == state.initial_event_pending
            && root_crossings_bit_eq(&self.pending_root_crossings, &state.pending_root_crossings)
            && self
                .indicator_storage
                .matches_snapshot(&state.indicator_storage)
            && self.pending_event_pre_y.bit_eq(&state.pending_event_pre_y)
            && self.pending_event_pre_p.bit_eq(&state.pending_event_pre_p)
            && self
                .boundary_event_pre_y
                .bit_eq(&state.boundary_event_pre_y)
            && self
                .boundary_event_pre_p
                .bit_eq(&state.boundary_event_pre_p)
            && float_slice_bit_eq(&self.solver_y_guess.borrow(), &state.solver_y_guess)
            && float_slice_bit_eq(
                &self.delay_params_scratch.borrow(),
                &state.delay_params_scratch,
            )
            && float_slice_bit_eq(
                &self.delay_solver_y_scratch.borrow(),
                &state.delay_solver_y_scratch,
            )
            && derivative_cache_bit_eq(&self.derivative_cache.borrow(), &state.derivative_cache)
            && continuous_linearization_cache_bit_eq(
                &self.continuous_linearization_cache.borrow(),
                &state.continuous_linearization_cache,
            )
            && option_float_bit_eq(self.max_step_duration, state.max_step_duration)
            && termination_bit_eq(self.termination.as_ref(), state.termination.as_ref())
            && self
                .settled_initialization_y
                .bit_eq(&state.settled_initialization_y)
            && self.runtime.matches_snapshot(&state.runtime)
    }

    /// Publish a successful detached event transaction into the existing
    /// construction-owned storage.
    ///
    /// Every operation before this call is fallible. These exact-size copies
    /// retain the preallocated indicator tables and buffers, value-reference
    /// brands, and runtime identity; no caller can observe an intermediate
    /// field assignment because the component is exclusively borrowed.
    pub(super) fn publish_event_stage(&mut self, staged: &mut Self) {
        self.stop_time = staged.stop_time;
        self.time = staged.time;
        self.set_time_bounds = staged.set_time_bounds;
        self.post_event_eval_time = staged.post_event_eval_time;
        self.event_anchor_time = staged.event_anchor_time;
        self.states.clone_from(&staged.states);
        self.params.clone_from(&staged.params);
        self.stop_schedule.clone_from(&staged.stop_schedule);
        self.pending_event_entry = staged.pending_event_entry;
        self.pending_state_event_entry = staged.pending_state_event_entry;
        self.pending_event_stop = staged.pending_event_stop;
        self.advance_state_to_event_right_limit = staged.advance_state_to_event_right_limit;
        self.state_time_coincidence = staged.state_time_coincidence;
        self.initial_event_pending = staged.initial_event_pending;
        self.pending_root_crossings
            .clone_from(&staged.pending_root_crossings);
        self.indicator_storage
            .copy_mutable_from(&staged.indicator_storage);
        self.pending_event_pre_y
            .copy_from(&staged.pending_event_pre_y);
        self.pending_event_pre_p
            .copy_from(&staged.pending_event_pre_p);
        self.boundary_event_pre_y
            .copy_from(&staged.boundary_event_pre_y);
        self.boundary_event_pre_p
            .copy_from(&staged.boundary_event_pre_p);
        self.event_solver_y_work
            .copy_from_slice(&staged.event_solver_y_work);
        self.event_state_before
            .copy_from_slice(&staged.event_state_before);
        self.scheduled_root_index_scratch
            .clone_from(&staged.scheduled_root_index_scratch);
        self.root_override_scratch
            .clone_from(&staged.root_override_scratch);
        self.solver_y_guess
            .borrow_mut()
            .clone_from(&staged.solver_y_guess.borrow());
        self.accepted_derivative_scratch
            .borrow_mut()
            .copy_from_slice(&staged.accepted_derivative_scratch.borrow());
        self.delay_params_scratch
            .borrow_mut()
            .clone_from(&staged.delay_params_scratch.borrow());
        self.delay_solver_y_scratch
            .borrow_mut()
            .clone_from(&staged.delay_solver_y_scratch.borrow());
        self.derivative_cache
            .borrow_mut()
            .copy_from(&staged.derivative_cache.borrow());
        self.continuous_linearization_cache
            .borrow_mut()
            .copy_from(&staged.continuous_linearization_cache.borrow());
        self.max_step_duration = staged.max_step_duration;
        self.termination.clone_from(&staged.termination);
        self.settled_initialization_y
            .copy_from(&staged.settled_initialization_y);
        #[cfg(test)]
        {
            self.verification_fail_next_exit_initialization =
                staged.verification_fail_next_exit_initialization;
            self.verification_fail_next_enter_initialization =
                staged.verification_fail_next_enter_initialization;
            self.verification_fail_next_update_discrete_states =
                staged.verification_fail_next_update_discrete_states;
            self.verification_fail_next_completed_integrator_step =
                staged.verification_fail_next_completed_integrator_step;
            self.verification_fail_next_enter_continuous_time_mode =
                staged.verification_fail_next_enter_continuous_time_mode;
        }
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
        self.public_time_eval_time(self.time)
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
            if cache.matches(time, &self.states, parameters) {
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
                    settle,
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
            .get(..self.state_domain.len())
            .ok_or_else(|| {
                contract(format!(
                    "observation solver vector has {} entries for {} state values",
                    solver_y.len(),
                    self.state_domain.len()
                ))
            })?
            .to_vec();
        if self.runtime.has_delay_channels() {
            self.runtime
                .refresh_delay_values(time, &solver_y, &mut parameters)
                .map_err(MeError::from)?;
        }
        write_observation_clock_activation_params(self.runtime.model(), &mut parameters);
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
            .model()
            .problem()
            .discrete()
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

    pub(super) fn solver_y_at_time_into(
        &self,
        time: f64,
        solver_y: &mut [f64],
    ) -> Result<(), MeError> {
        if solver_y.len() != self.solver_y_guess.borrow().len() {
            return Err(contract(format!(
                "event solver buffer has {} entries for constructed width {}",
                solver_y.len(),
                self.solver_y_guess.borrow().len()
            )));
        }
        solver_y.copy_from_slice(&self.solver_y_guess.borrow());
        let settle = self.numerics_settle();
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.runtime
                .full_solver_y_with_guess(
                    time,
                    &self.states,
                    params,
                    solver_y,
                    settle.tol,
                    settle.max_iters,
                )
                .map_err(MeError::from)
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

    pub(super) fn refresh_current_delay_facts(&mut self) -> Result<(), MeError> {
        if !self.runtime.has_delay_channels() {
            return Ok(());
        }
        let settle = self.numerics_settle();
        let mut solver_y = self.solver_y_guess.borrow_mut();
        if solver_y.len() < self.states.len() {
            return Err(contract(format!(
                "delay refresh solver vector has {} entries for {} state values",
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
        Ok(())
    }

    pub(super) fn commit_delay_point(&mut self) -> Result<(), MeError> {
        self.refresh_current_delay_facts()?;
        let solver_y = self.solver_y_guess.borrow();
        self.runtime
            .commit_delay_history(self.time, &solver_y, &self.params)?;
        Ok(())
    }

    // -- caches ------------------------------------------------------------

    fn copy_cached_derivative_into(&self, time: f64, state: &[f64], out: &mut [f64]) -> bool {
        let cache = self.derivative_cache.borrow();
        if !cache.valid
            || cache.time.to_bits() != time.to_bits()
            || !state_values_match(&cache.state, state)
            || cache.derivative.len() != out.len()
        {
            return false;
        }
        out.copy_from_slice(&cache.derivative);
        true
    }

    #[cfg(test)]
    pub(crate) fn cached_derivative(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        let cache = self.derivative_cache.borrow();
        (cache.valid
            && cache.time.to_bits() == time.to_bits()
            && state_values_match(&cache.state, state))
        .then(|| cache.derivative.clone())
    }

    pub(crate) fn cache_derivative(&self, time: f64, state: &[f64], derivative: &[f64]) {
        let mut cache = self.derivative_cache.borrow_mut();
        cache.valid = true;
        cache.time = time;
        cache.state.clone_from_slice(state);
        cache.derivative.clone_from_slice(derivative);
    }

    pub(super) fn clear_runtime_caches(&self) {
        self.clear_callback_value_caches();
        self.continuous_linearization_cache.borrow_mut().valid = false;
    }

    pub(super) fn clear_callback_value_caches(&self) {
        self.derivative_cache.borrow_mut().valid = false;
        self.indicator_storage.clear_cache();
    }

    pub(super) fn cache_continuous_linearization(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        _solver_y: &[f64],
    ) {
        self.indicator_storage.clear_cache();
        let mut cache = self.continuous_linearization_cache.borrow_mut();
        cache.valid = true;
        cache.time = time;
        cache.state.clone_from_slice(state);
        cache.parameters.clone_from_slice(parameters);
    }

    fn invalidate_continuous_linearization(&self) {
        self.continuous_linearization_cache.borrow_mut().valid = false;
    }

    fn continuous_linearization_cache_matches(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) -> bool {
        self.continuous_linearization_cache
            .borrow()
            .matches(time, state, parameters)
    }

    fn cached_continuous_solver_y(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) -> Option<Vec<f64>> {
        self.continuous_linearization_cache
            .borrow()
            .matches(time, state, parameters)
            .then_some(())
            .map(|_| self.solver_y_guess.borrow().clone())
    }

    // -- initialization ----------------------------------------------------

    /// `fmi3EnterInitializationMode`, unannotated; the trait method attaches
    /// [`MeStage::Initialization`].
    pub(super) fn enter_initialization_mode_inner(
        &mut self,
        start_time: f64,
    ) -> Result<(), MeError> {
        if !start_time.is_finite() || start_time > self.stop_time {
            return Err(contract(format!(
                "initialization start must be finite and no later than stop time {}; got {start_time}",
                self.stop_time,
            )));
        }
        self.time = start_time;
        self.set_time_bounds = MeSetTimeBounds::at_start(start_time);
        self.post_event_eval_time = None;
        self.event_anchor_time = start_time;
        self.stop_schedule =
            SolveStopSchedule::new(self.runtime.model().problem(), start_time, self.stop_time);
        self.pending_event_entry = None;
        self.pending_state_event_entry = false;
        self.pending_event_stop = None;
        self.advance_state_to_event_right_limit = false;
        self.state_time_coincidence = StateTimeCoincidence::None;
        self.initial_event_pending = false;
        self.pending_root_crossings.clear();
        self.pending_event_pre_y.clear();
        self.pending_event_pre_p.clear();
        self.boundary_event_pre_y.clear();
        self.boundary_event_pre_p.clear();
        self.settled_initialization_y.clear();
        self.termination = None;
        self.clear_runtime_caches();
        self.runtime.reset_delay_history();
        self.runtime.initialize_delay_history(
            start_time,
            self.runtime.model().initial_y(),
            &mut self.params,
        )?;
        self.runtime.set_initial_event_flag(&mut self.params, true);
        Ok(())
    }

    /// `fmi3ExitInitializationMode`, unannotated; the trait method attaches
    /// [`MeStage::Initialization`].
    pub(super) fn exit_initialization_mode_inner(&mut self) -> Result<(), MeError> {
        let mut solver_y = std::mem::take(&mut self.event_solver_y_work);
        let result = self.exit_initialization_mode_with_storage(&mut solver_y);
        self.event_solver_y_work = solver_y;
        result
    }

    fn exit_initialization_mode_with_storage(
        &mut self,
        solver_y: &mut [f64],
    ) -> Result<(), MeError> {
        self.solver_y_at_time_into(self.public_time_eval_time(self.time), solver_y)?;
        let policy = self.algebraic_projection_policy();
        let settle = policy.settle;
        self.runtime.settle_initialization_system(
            solver_y,
            &mut self.params,
            self.time,
            self.tolerance,
            settle.max_iters,
        )?;
        project_algebraics(&self.runtime, solver_y, &mut self.params, self.time, policy)?;
        self.copy_states_from_solver_y(solver_y);
        self.runtime.update_relation_memory_from_state(
            self.time,
            &self.states,
            &mut self.params,
            self.tolerance,
            settle.max_iters,
        )?;
        self.copy_states_from_solver_y(solver_y);
        self.invalidate_continuous_linearization();
        self.solver_y_guess.borrow_mut().copy_from_slice(solver_y);
        // MLS 3.6 §8.6: before integration, v = pre(v). The initial event
        // therefore reads the values the initialization system just settled,
        // never the declared starts that seeded that solve.
        self.pending_event_pre_y
            .set(solver_y, "initial event-entry solver latch")?;
        self.pending_event_pre_p
            .set(&self.params, "initial event-entry parameter latch")?;
        self.settled_initialization_y
            .set(solver_y, "settled initialization solver latch")?;
        self.initial_event_pending = true;
        Ok(())
    }

    // -- event boundary ----------------------------------------------------

    pub(super) fn apply_discrete_event_updates(
        &mut self,
        event_time: f64,
        _event: RuntimeEventStop,
        row_filter: EventUpdateRowFilter,
        refresh_iteration_y: bool,
    ) -> Result<(), MeError> {
        let mut solver_y = std::mem::take(&mut self.event_solver_y_work);
        let mut root_overrides = std::mem::take(&mut self.root_override_scratch);
        root_overrides.clear();
        root_overrides.extend(
            self.pending_root_crossings
                .drain(..)
                .map(|crossing| (crossing.index, crossing.post_relation_memory_value)),
        );
        let result = self.apply_discrete_event_updates_with_storage(
            event_time,
            row_filter,
            refresh_iteration_y,
            &mut solver_y,
            &root_overrides,
        );
        self.event_solver_y_work = solver_y;
        self.root_override_scratch = root_overrides;
        result
    }

    fn apply_discrete_event_updates_with_storage(
        &mut self,
        event_time: f64,
        row_filter: EventUpdateRowFilter,
        refresh_iteration_y: bool,
        solver_y: &mut [f64],
        root_overrides: &[(usize, f64)],
    ) -> Result<(), MeError> {
        if refresh_iteration_y {
            self.solver_y_at_time_into(self.public_time_eval_time(self.time), solver_y)?;
        } else {
            solver_y.copy_from_slice(
                self.pending_event_pre_y
                    .get("event update requires a latched pre-event solver vector")?,
            );
        }
        let event_entry_y = self
            .pending_event_pre_y
            .get("event update requires a latched pre-event solver vector")?;
        let event_entry_p = self
            .pending_event_pre_p
            .get("event update requires a latched pre-event parameter vector")?;
        let runtime = Rc::clone(&self.runtime);
        let projection_runtime = Rc::clone(&runtime);
        let policy = self.algebraic_projection_policy();
        let tol = policy.tolerance;
        let settle = policy.settle;
        let outcome = runtime.apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: solver_y,
                p: &mut self.params,
                t: event_time,
                tol,
                event_pre_y: event_entry_y,
                event_pre_p: event_entry_p,
                max_iters: settle.max_iters,
                row_filter,
                root_relation_overrides: root_overrides,
            },
            move |y, p| project_event_algebraics(&projection_runtime, y, p, event_time, policy),
        )?;
        // Unrelated algebraic/output lanes remain lazy in the retained solver
        // seed. Their owning callback refresh plan materializes them if and
        // when a derivative, root, or visible-value consumer asks for them.
        self.pending_event_pre_y.clear();
        self.pending_event_pre_p.clear();
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

    pub(super) fn commit_event_runtime_state(
        &mut self,
        event_time: f64,
        solver_y: &mut [f64],
        root_overrides: &[(usize, f64)],
    ) -> Result<(), MeError> {
        let history_changed = commit_pre_params_after_event_at(
            self.runtime.model(),
            solver_y,
            &mut self.params,
            Some(event_time),
            self.tolerance,
        );
        if history_changed {
            self.canonicalize_committed_event_view(event_time, solver_y, root_overrides)?;
        }
        self.copy_states_from_solver_y(solver_y);
        self.invalidate_continuous_linearization();
        self.solver_y_guess.borrow_mut().copy_from_slice(solver_y);
        if history_changed {
            let solver_y = self.solver_y_guess.borrow();
            self.cache_continuous_linearization(event_time, &self.states, &self.params, &solver_y);
        }
        self.commit_delay_point()?;
        Ok(())
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
        _event_time: f64,
    ) -> Result<(), MeError> {
        match outcome {
            EventActionOutcome::Continue => Ok(()),
            EventActionOutcome::AssertionFailed { time, message: _ } if !time.is_finite() => {
                Err(MeError::NonFiniteEventActionTime {
                    action: "assert",
                    time,
                })
            }
            EventActionOutcome::AssertionFailed { time, message } => {
                Err(MeError::Assertion { time, message })
            }
            EventActionOutcome::Terminated { time, .. } if !time.is_finite() => {
                Err(MeError::NonFiniteEventActionTime {
                    action: "terminate",
                    time,
                })
            }
            EventActionOutcome::Terminated { time, message } => {
                self.termination
                    .get_or_insert(SimTermination { time, message });
                Ok(())
            }
        }
    }

    pub(super) fn prepare_event_pre_for_update(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), MeError> {
        if self.pending_event_pre_y.is_occupied() {
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
                self.pending_event_pre_y
                    .get_mut("event pre-state is unavailable")?[..self.state_domain.len()]
                    .copy_from_slice(&self.states);
            }
            self.pending_event_pre_p
                .get("event pre-state has no paired parameter latch")?;
            return Ok(());
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
        let mut solver_y = std::mem::take(&mut self.event_solver_y_work);
        let result = self
            .solver_y_at_time_into(pre_time, &mut solver_y)
            .and_then(|()| {
                self.pending_event_pre_y
                    .set(&solver_y, "computed event-entry solver latch")
            })
            .and_then(|()| {
                self.pending_event_pre_p
                    .set(&self.params, "computed event-entry parameter latch")
            });
        self.event_solver_y_work = solver_y;
        result
    }

    pub(super) fn clear_event_entry_scheduled_root_relation_memory(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), MeError> {
        if event.observe_right_limit || !matches!(event.pre_mode, EventPreMode::EventEntry) {
            return Ok(());
        }
        self.fill_scheduled_root_indices_at_time(event_time);
        clear_scheduled_root_relation_memory(
            self.runtime.model(),
            &self.scheduled_root_index_scratch,
            &mut self.params,
        )
        .map_err(contract)
    }

    pub(super) fn clear_all_scheduled_root_relation_memory(&mut self) -> Result<(), MeError> {
        self.scheduled_root_index_scratch.clear();
        self.scheduled_root_index_scratch.extend(
            self.runtime
                .model()
                .problem()
                .events()
                .scheduled_root_conditions
                .iter()
                .map(|root| root.root_index),
        );
        clear_scheduled_root_relation_memory(
            self.runtime.model(),
            &self.scheduled_root_index_scratch,
            &mut self.params,
        )
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
        self.fill_scheduled_root_indices_at_time(event_time);
        for index in self.scheduled_root_index_scratch.iter().copied() {
            self.pending_root_crossings.push(RootCrossing {
                index,
                post_relation_memory_value: 1.0,
            });
        }
    }

    fn fill_scheduled_root_indices_at_time(&mut self, event_time: f64) {
        self.scheduled_root_index_scratch.clear();
        self.scheduled_root_index_scratch.extend(
            self.runtime
                .model()
                .problem()
                .events()
                .scheduled_root_conditions
                .iter()
                .filter(|root| timeline::scheduled_root_matches_time(root, event_time))
                .map(|root| root.root_index),
        );
    }

    pub(super) fn run_initial_event_boundary(&mut self) -> Result<MeDiscreteStates, MeError> {
        self.event_state_before.copy_from_slice(&self.states);
        let event_time = self.time;
        let runtime = Rc::clone(&self.runtime);
        let projection_runtime = Rc::clone(&runtime);
        let policy = self.algebraic_projection_policy();
        let tol = policy.tolerance;
        let settle = policy.settle;
        let settled_initialization_y = self
            .settled_initialization_y
            .get("initial event boundary requires a settled solver vector")?;
        let startup_event_pre_y = self
            .pending_event_pre_y
            .get("initial event boundary requires a latched pre-event state")?;
        let startup_event_pre_p = self
            .pending_event_pre_p
            .get("initial event boundary requires a latched pre-event parameter vector")?;
        let mut solver_y = std::mem::take(&mut self.event_solver_y_work);
        solver_y.copy_from_slice(settled_initialization_y);
        self.settled_initialization_y.clear();
        let dynamic_event_result =
            self.runtime
                .current_dynamic_time_event_stop(&solver_y, &self.params, self.time);
        let dynamic_event = match dynamic_event_result {
            Ok(event) => event,
            Err(error) => {
                self.event_solver_y_work = solver_y;
                return Err(error.into());
            }
        };
        let outcome_result = runtime.apply_projected_initial_event_boundary(
            ProjectedInitialEventInput {
                y: &mut solver_y,
                p: &mut self.params,
                t_start: self.time,
                t_end: self.stop_time,
                tol,
                event_pre_y: startup_event_pre_y,
                event_pre_p: startup_event_pre_p,
                max_iters: settle.max_iters,
                dynamic_event,
            },
            move |y, p, t| project_algebraics(&projection_runtime, y, p, t, policy),
        );
        let outcome = match outcome_result {
            Ok(outcome) => outcome,
            Err(error) => {
                self.event_solver_y_work = solver_y;
                return Err(error.into());
            }
        };
        self.pending_event_pre_y.clear();
        self.pending_event_pre_p.clear();
        self.copy_states_from_solver_y(&solver_y);
        self.invalidate_continuous_linearization();
        self.solver_y_guess.borrow_mut().copy_from_slice(&solver_y);
        // The line above is the last read of the taken workspace. The single
        // construction-reserved buffer returns to its field here, before any
        // consumer runs: `discrete_states_after_update` takes the same field
        // to evaluate a solver-reading dynamic time event, and it must find
        // the sized storage, not the empty vector left by `mem::take`.
        self.event_solver_y_work = solver_y;
        self.time = outcome.final_t;
        self.record_event_action_outcome(outcome.action, event_time)?;
        self.initial_event_pending = false;
        let right_limit = (outcome.final_t > event_time).then_some(outcome.final_t);
        self.time = event_time;
        self.set_post_event_eval_time(right_limit);
        self.discrete_states_after_update(continuous_state_values_changed(
            &self.event_state_before,
            &self.states,
        ))
    }

    pub(super) fn run_runtime_event_boundary(
        &mut self,
        entry: PendingEventEntry,
    ) -> Result<MeDiscreteStates, MeError> {
        self.event_state_before.copy_from_slice(&self.states);
        let tolerance = self.tolerance.max(1.0e-10);
        match entry.cause {
            PendingEventCause::State => {
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
                    &self.event_state_before,
                    &self.states,
                ))
            }
            PendingEventCause::Time => {
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
                    &self.event_state_before,
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
            let reads_solver_y = self.runtime.dynamic_time_event_stop_reads_solver_y();
            let mut solver_y = std::mem::take(&mut self.event_solver_y_work);
            let result = if reads_solver_y {
                match self
                    .solver_y_at_time_into(self.public_time_eval_time(self.time), &mut solver_y)
                {
                    Ok(()) => self
                        .runtime
                        .next_runtime_event_stop(
                            &solver_y,
                            &self.params,
                            &mut self.stop_schedule,
                            self.time,
                            self.stop_time,
                        )
                        .map_err(MeError::from),
                    Err(error) => Err(error),
                }
            } else {
                self.runtime
                    .next_runtime_event_stop(
                        &[],
                        &self.params,
                        &mut self.stop_schedule,
                        self.time,
                        self.stop_time,
                    )
                    .map_err(MeError::from)
            };
            self.event_solver_y_work = solver_y;
            let (time, event) = result?;
            self.pending_event_stop = event.map(|event| (time, event));
            event.map(|_| time)
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

/// The scalar value one resolved indicator position reports.
fn indicator_reading_value(
    reading: IndicatorReading,
    time: f64,
    root_values: &RootIndicatorValues,
    deadlines: &DeadlineIndicatorValues,
) -> f64 {
    match reading {
        IndicatorReading::RootValue { index } => root_values.as_slice()[index],
        IndicatorReading::DeadlineDistance { index } => deadlines.as_slice()[index] - time,
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
