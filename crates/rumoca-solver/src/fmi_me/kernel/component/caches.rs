//! Exact-coordinate callback and settled-view cache identities.

use super::*;

impl SolveMeKernel {
    // -- caches ------------------------------------------------------------

    pub(super) fn copy_cached_derivative_into(
        &self,
        time: f64,
        state: &[f64],
        out: &mut [f64],
    ) -> bool {
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

    pub(in crate::fmi_me::kernel) fn clear_derivative_cache(&self) {
        *self.derivative_cache.borrow_mut() = None;
        *self.continuous_linearization_cache.borrow_mut() = None;
    }

    pub(in crate::fmi_me::kernel) fn clear_runtime_caches(&self) {
        self.clear_callback_value_caches();
        *self.continuous_linearization_cache.borrow_mut() = None;
    }

    pub(in crate::fmi_me::kernel) fn clear_callback_value_caches(&self) {
        *self.derivative_cache.borrow_mut() = None;
        *self.root_cache.borrow_mut() = None;
    }

    pub(super) fn copy_cached_root_conditions_into(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        out: &mut [f64],
    ) -> bool {
        let cache = self.root_cache.borrow();
        let Some(cached) = cache.as_ref() else {
            return false;
        };
        if cached.time.to_bits() != time.to_bits()
            || !state_values_match(&cached.state, state)
            || !state_values_match(&cached.parameters, parameters)
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
        (cached.time.to_bits() == time.to_bits()
            && state_values_match(&cached.state, state)
            && state_values_match(&cached.parameters, &self.params))
        .then(|| cached.values.clone())
    }

    #[cfg(test)]
    pub(crate) fn cache_root_conditions(&self, time: f64, state: &[f64], values: &[f64]) {
        self.cache_root_conditions_at_parameters(time, state, &self.params, values);
    }

    pub(super) fn cache_root_conditions_at_parameters(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        values: &[f64],
    ) {
        let mut cache = self.root_cache.borrow_mut();
        if let Some(cached) = cache.as_mut() {
            cached.time = time;
            cached.state.clone_from_slice(state);
            cached.parameters.clone_from_slice(parameters);
            cached.values.clone_from_slice(values);
        } else {
            *cache = Some(CachedRootConditions {
                time,
                state: state.to_vec(),
                parameters: parameters.to_vec(),
                values: values.to_vec(),
            });
        }
    }

    pub(in crate::fmi_me::kernel) fn cache_continuous_linearization(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        solver_y: &[f64],
    ) {
        self.cache_settled_coordinate(
            time,
            state,
            parameters,
            solver_y,
            SettledCoordinateKind::Derivative,
        );
    }

    pub(super) fn cache_algebraic_coordinate(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        solver_y: &[f64],
    ) {
        self.cache_settled_coordinate(
            time,
            state,
            parameters,
            solver_y,
            SettledCoordinateKind::Algebraic,
        );
    }

    pub(super) fn cache_settled_coordinate(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        solver_y: &[f64],
        kind: SettledCoordinateKind,
    ) {
        let mut cache = self.continuous_linearization_cache.borrow_mut();
        if let Some(cached) = cache.as_mut() {
            cached.kind = kind;
            cached.solver_y.clone_from_slice(solver_y);
            cached.time = time;
            cached.state.clone_from_slice(state);
            cached.parameters.clone_from_slice(parameters);
        } else {
            *cache = Some(CachedContinuousLinearization {
                kind,
                solver_y: solver_y.to_vec(),
                time,
                state: state.to_vec(),
                parameters: parameters.to_vec(),
            });
        }
    }

    pub(in crate::fmi_me::kernel) fn invalidate_continuous_linearization(&self) {
        *self.continuous_linearization_cache.borrow_mut() = None;
    }

    #[cfg(test)]
    pub(super) fn continuous_linearization_cache_matches(
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

    pub(super) fn copy_cached_algebraic_solver_y(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        out: &mut [f64],
    ) -> bool {
        let cache = self.continuous_linearization_cache.borrow();
        let Some(cached) = cache.as_ref().filter(|cached| {
            cached.kind == SettledCoordinateKind::Algebraic
                && cached.matches(time, state, parameters)
                && cached.solver_y.len() == out.len()
        }) else {
            return false;
        };
        out.copy_from_slice(&cached.solver_y);
        true
    }

    pub(super) fn cached_continuous_solver_y(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) -> Option<SettledIndicatorCoordinate> {
        self.continuous_linearization_cache
            .borrow()
            .as_ref()
            .filter(|cached| cached.matches(time, state, parameters))
            .map(|cached| SettledIndicatorCoordinate {
                kind: cached.kind,
                values: cached.solver_y.clone(),
            })
    }
}
