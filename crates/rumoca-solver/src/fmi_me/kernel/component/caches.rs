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

    /// Stamp the solver guess as the coordinate a derivative refresh settled
    /// at `(time, state, parameters)`. The refresh wrote it in place, so the
    /// stamp names the guess as it stands; the caller writes nothing more
    /// under the borrow it settled in.
    pub(in crate::fmi_me::kernel) fn cache_continuous_linearization(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) {
        self.cache_settled_coordinate(
            time,
            state,
            parameters,
            None,
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
            Some(solver_y),
            SettledCoordinateKind::Algebraic,
        );
    }

    /// `solver_y` is copied; `None` stamps the solver guess instead. The
    /// retained buffers are reused.
    fn cache_settled_coordinate(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        solver_y: Option<&[f64]>,
        kind: SettledCoordinateKind,
    ) {
        let mut cache = self.continuous_linearization_cache.borrow_mut();
        let (mut retained_state, mut retained_parameters, retained_values) = match cache.take() {
            Some(cached) => (
                cached.state,
                cached.parameters,
                match cached.values {
                    SettledValues::Owned(values) => values,
                    SettledValues::Guess { .. } => Vec::new(),
                },
            ),
            None => Default::default(),
        };
        let values = match solver_y {
            None => SettledValues::Guess {
                writes: self.solver_y_guess.writes(),
            },
            Some(solver_y) => {
                let mut values = retained_values;
                overwrite(&mut values, solver_y);
                SettledValues::Owned(values)
            }
        };
        overwrite(&mut retained_state, state);
        overwrite(&mut retained_parameters, parameters);
        *cache = Some(CachedContinuousLinearization {
            kind,
            values,
            time,
            state: retained_state,
            parameters: retained_parameters,
        });
    }

    /// The settled values `cached` names, unless its stamp is stale.
    pub(super) fn settled_values<'a>(
        &'a self,
        cached: &'a CachedContinuousLinearization,
    ) -> Option<SettledView<'a>> {
        match &cached.values {
            SettledValues::Owned(values) => Some(SettledView::Owned(values)),
            SettledValues::Guess { writes } => (*writes == self.solver_y_guess.writes())
                .then(|| SettledView::Guess(self.solver_y_guess.borrow())),
        }
    }

    /// The settled coordinate a snapshot keeps: a stale stamp names nothing.
    pub(in crate::fmi_me::kernel) fn snapshot_settled_coordinate(
        &self,
    ) -> Option<CachedContinuousLinearization> {
        let cache = self.continuous_linearization_cache.borrow();
        cache
            .as_ref()
            .filter(|cached| match cached.values {
                SettledValues::Owned(_) => true,
                SettledValues::Guess { writes } => writes == self.solver_y_guess.writes(),
            })
            .cloned()
    }

    /// Restore a snapshot's settled coordinate after its solver guess: a
    /// stamp then names the restored guess.
    pub(in crate::fmi_me::kernel) fn restore_settled_coordinate(
        &self,
        saved: Option<&CachedContinuousLinearization>,
    ) {
        let mut restored = saved.cloned();
        if let Some(SettledValues::Guess { writes }) =
            restored.as_mut().map(|cached| &mut cached.values)
        {
            *writes = self.solver_y_guess.writes();
        }
        *self.continuous_linearization_cache.borrow_mut() = restored;
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
            .is_some_and(|cached| {
                cached.matches(time, state, parameters) && self.settled_values(cached).is_some()
            })
    }

    pub(super) fn copy_cached_algebraic_solver_y(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        out: &mut [f64],
    ) -> bool {
        let cache = self.continuous_linearization_cache.borrow();
        // An algebraic settle is always a copy, so this never borrows the
        // guess `out` may be.
        let Some(SettledValues::Owned(values)) = cache
            .as_ref()
            .filter(|cached| {
                cached.kind == SettledCoordinateKind::Algebraic
                    && cached.matches(time, state, parameters)
            })
            .map(|cached| &cached.values)
        else {
            return false;
        };
        if values.len() != out.len() {
            return false;
        }
        out.copy_from_slice(values);
        true
    }

    pub(super) fn cached_continuous_solver_y(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
    ) -> Option<SettledIndicatorCoordinate> {
        let cache = self.continuous_linearization_cache.borrow();
        let cached = cache
            .as_ref()
            .filter(|cached| cached.matches(time, state, parameters))?;
        let values = self.settled_values(cached)?.to_vec();
        Some(SettledIndicatorCoordinate {
            kind: cached.kind,
            values,
        })
    }
}

/// A borrowed settled coordinate: a copy, or the stamped solver guess.
pub(super) enum SettledView<'a> {
    Owned(&'a [f64]),
    Guess(std::cell::Ref<'a, Vec<f64>>),
}

impl std::ops::Deref for SettledView<'_> {
    type Target = [f64];

    fn deref(&self) -> &[f64] {
        match self {
            Self::Owned(values) => values,
            Self::Guess(values) => values,
        }
    }
}

/// Overwrite a retained buffer, keeping its allocation.
fn overwrite(buffer: &mut Vec<f64>, source: &[f64]) {
    buffer.clear();
    buffer.extend_from_slice(source);
}
