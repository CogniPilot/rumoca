//! The kinematic derivative entry of the component: the derivatives of the
//! Solve IR kinematic states, read at the coordinate a derivative evaluation
//! settled, without an algebraic refresh.

use super::*;

impl SolveMeKernel {
    /// The derivatives of the kinematic states at the current time and states,
    /// written into their entries of `derivatives`; the other entries are not
    /// written.
    ///
    /// No algebraic refresh runs. The coordinate the last derivative refresh
    /// settled must be at this evaluation time and these parameters, and may
    /// differ from the current states only outside the kinematic set; its
    /// algebraics are reused with the current states written over its state
    /// prefix. The Solve IR partition makes that exact: a kinematic row reads
    /// the other states only as slots, and no algebraic it reads depends on
    /// them. Anything else is a contract failure, never a refresh. The rows
    /// read the stamped solver guess in place, with the current states over
    /// its prefix only for the evaluation.
    pub(crate) fn kinematic_state_derivatives_into(
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
        self.require_active_lifecycle("get_kinematic_state_derivatives")?;
        let kinematic = self.kinematic_state_indices();
        let time = self.continuous_eval_time();
        if !self.settled_kinematic_coordinate(time, kinematic) {
            return Err(contract(
                "kinematic state derivatives need the settled coordinate of a derivative \
                 evaluation at this time and these kinematic states",
            ));
        }
        let mut states = self.states.clone();
        let mut values = vec![0.0; self.state_count];
        self.solver_y_guess
            .with_prefix(&mut states, |solver_y| {
                self.runtime.eval_derivative_rows_at_settled_y(
                    time,
                    solver_y,
                    &self.params,
                    kinematic,
                    &mut values,
                )
            })
            .ok_or_else(|| contract("the settled solver vector is in use"))?
            .map_err(|error| MeError::from(error).at_stage(MeStage::Integration))?;
        for &state in kinematic {
            derivatives[state] = values[state];
        }
        Ok(())
    }

    /// Whether the solver guess holds the coordinate the last derivative
    /// refresh settled at `time`, the current parameters and the current
    /// kinematic states.
    fn settled_kinematic_coordinate(&self, time: f64, kinematic: &[usize]) -> bool {
        let cache = self.continuous_linearization_cache.borrow();
        let Some(cached) = cache.as_ref() else {
            return false;
        };
        let kinematic_match = |state: &usize| {
            cached.state.get(*state).map(|value| value.to_bits())
                == self.states.get(*state).map(|value| value.to_bits())
        };
        cached.kind == SettledCoordinateKind::Derivative
            && cached.time.to_bits() == time.to_bits()
            && state_values_match(&cached.parameters, &self.params)
            && kinematic.iter().all(kinematic_match)
            && matches!(cached.values, SettledValues::Guess { .. })
            && self
                .settled_values(cached)
                .is_some_and(|values| values.len() >= self.state_count)
    }

    /// The Solve IR kinematic states this component serves: none when delay
    /// values in the parameters follow the coordinate, the derivative program
    /// changes with the reduced chart, or the runtime has no scalar
    /// derivative program to run without a refresh.
    pub(crate) fn kinematic_state_indices(&self) -> &[usize] {
        if self.reduced_charts.is_some()
            || self.runtime.has_delay_channels()
            || self.runtime.scalar_execution().is_err()
        {
            return &[];
        }
        self.runtime
            .model()
            .artifacts
            .continuous
            .structural
            .kinematic_states()
    }
}
