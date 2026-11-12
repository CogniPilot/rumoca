//! What tests and the input-write audit observe of the component's internal
//! state.

use super::*;

impl SolveMeKernel {
    /// The identity of the resolved indicator table and of the storage every
    /// indicator read uses.
    ///
    /// The plan shape comes from the constructor and the buffer addresses are
    /// the ones reserved with it, so a step that rebuilt or regrew either would
    /// change this value.
    #[cfg(test)]
    pub(crate) fn verification_indicator_storage(&self) -> IndicatorStorageIdentity {
        let mut domain_buffers = [
            self.frozen_indicator_positive.as_ptr() as usize,
            self.indicator_domain_scratch.as_ptr() as usize,
        ];
        domain_buffers.sort_unstable();
        IndicatorStorageIdentity {
            entries: self
                .indicator_plan
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
            root_value_len: self.indicator_plan.root_value_len(),
            deadline_len: self.indicator_plan.deadline_len(),
            root_buffer: self.indicator_root_scratch.borrow().as_ptr() as usize,
            deadline_buffer: self.indicator_deadline_scratch.borrow().as_ptr() as usize,
            value_buffer: self.indicator_value_scratch.as_ptr() as usize,
            domain_buffers,
        }
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
        self.canonicalize_committed_event_view(event_time, solver_y, &mut Vec::new())
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
        self.cache_continuous_linearization(time, state, parameters);
    }

    /// A write to the solver guess that leaves its values as they are.
    #[cfg(test)]
    pub(crate) fn verification_touch_solver_guess(&self) {
        drop(self.solver_y_guess.borrow_mut());
    }

    /// Invert every frozen indicator domain, leaving the coordinate as it is.
    #[cfg(test)]
    pub(crate) fn verification_invert_frozen_indicator_domains(&mut self) {
        for positive in &mut self.frozen_indicator_positive {
            *positive = !*positive;
        }
    }

    #[cfg(any(test, feature = "input-write-audit"))]
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
                self.snapshot_settled_coordinate().as_ref(),
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
            && self.active_chart == state.active_chart
            && self.active_reference.map(f64::to_bits) == state.active_reference.map(f64::to_bits)
            && self.chart_runtimes_match_snapshots(&state.runtimes)
    }

    /// Whether every reduced-chart runtime matches a saved snapshot, in chart
    /// index order. A model with no folding first-integral group compares its
    /// single primary runtime against the single saved snapshot.
    #[cfg(any(test, feature = "input-write-audit"))]
    fn chart_runtimes_match_snapshots(
        &self,
        snapshots: &[Option<crate::runtime::solve_runtime::SolveRuntimeSnapshot>],
    ) -> bool {
        match &self.reduced_charts {
            None => snapshots
                .first()
                .and_then(Option::as_ref)
                .is_some_and(|snapshot| self.runtime.as_ref().matches_snapshot(snapshot)),
            Some(charts) => charts.match_snapshots(snapshots),
        }
    }
}
