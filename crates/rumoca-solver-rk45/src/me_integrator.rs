//! Dormand-Prince 5(4) behind the solver-neutral FMI ME plugin contract.
//!
//! This module owns only numerical-method state: the tableau, adaptive step
//! controller, and the most recently accepted continuous extension. FMI
//! lifecycle, event/root handling, output cadence, tracing, and component
//! policy remain in the common host.

use rumoca_solver::fmi_me::{
    MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError,
    MeIntegratorBackend, MeNumericalFailure, MeNumericalSetup, MeStepCandidate,
    accepted_interval_contains,
};

use crate::dense_output::Dopri5DenseOutput;

const METHOD: &str = "rk45";
const MIN_STEP: f64 = 1.0e-12;
const CONTINUOUS_EXTENSION_ORDER: u32 = 4;

/// Build the RK45 numerical plugin accepted by the common FMI ME host.
///
/// The returned trait object exposes no RK-specific structure to the host.
#[must_use]
pub fn model_exchange_integrator(
    setup: MeNumericalSetup,
) -> Box<dyn MeIntegratorBackend + 'static> {
    Box::new(Rk45Integrator::new(setup))
}

struct Rk45Integrator {
    relative_tolerance: f64,
    state_absolute_tolerances: Vec<f64>,
    initial_step: f64,
    next_step: f64,
    derivatives: Option<MeDerivativeHandle>,
    accepted_interval: Option<Dopri5DenseOutput>,
}

impl Rk45Integrator {
    fn new(setup: MeNumericalSetup) -> Self {
        let absolute_tolerance = setup.absolute_tolerance();
        let state_absolute_tolerances = setup
            .state_nominals()
            .iter()
            .map(|nominal| (absolute_tolerance * nominal).clamp(f64::MIN_POSITIVE, f64::MAX))
            .collect();
        let initial_step = setup.initial_step_hint().unwrap_or(1.0e-3);
        Self {
            relative_tolerance: setup.relative_tolerance(),
            state_absolute_tolerances,
            initial_step,
            next_step: initial_step,
            derivatives: None,
            accepted_interval: None,
        }
    }

    fn derivative(&self, time: f64, states: &[f64]) -> Result<Vec<f64>, MeIntegrationError> {
        let derivatives = self.derivatives.as_ref().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                "the host has not initialized the derivative capability",
            )
        })?;
        derivatives.derivatives(time, states).map_err(Into::into)
    }

    fn trial_step(
        &self,
        time: f64,
        state: &[f64],
        step: f64,
    ) -> Result<TrialStep, MeIntegrationError> {
        let k1 = self.derivative(time, state)?;
        let y2 = combine_stage(state, step, &[(&k1, 1.0 / 5.0)])?;
        let k2 = self.derivative(time + step * (1.0 / 5.0), &y2)?;

        let y3 = combine_stage(state, step, &[(&k1, 3.0 / 40.0), (&k2, 9.0 / 40.0)])?;
        let k3 = self.derivative(time + step * (3.0 / 10.0), &y3)?;

        let y4 = combine_stage(
            state,
            step,
            &[(&k1, 44.0 / 45.0), (&k2, -56.0 / 15.0), (&k3, 32.0 / 9.0)],
        )?;
        let k4 = self.derivative(time + step * (4.0 / 5.0), &y4)?;

        let y5 = combine_stage(
            state,
            step,
            &[
                (&k1, 19372.0 / 6561.0),
                (&k2, -25360.0 / 2187.0),
                (&k3, 64448.0 / 6561.0),
                (&k4, -212.0 / 729.0),
            ],
        )?;
        let k5 = self.derivative(time + step * (8.0 / 9.0), &y5)?;

        let y6 = combine_stage(
            state,
            step,
            &[
                (&k1, 9017.0 / 3168.0),
                (&k2, -355.0 / 33.0),
                (&k3, 46732.0 / 5247.0),
                (&k4, 49.0 / 176.0),
                (&k5, -5103.0 / 18656.0),
            ],
        )?;
        let k6 = self.derivative(time + step, &y6)?;

        let fifth_order = combine_stage(
            state,
            step,
            &[
                (&k1, 35.0 / 384.0),
                (&k3, 500.0 / 1113.0),
                (&k4, 125.0 / 192.0),
                (&k5, -2187.0 / 6784.0),
                (&k6, 11.0 / 84.0),
            ],
        )?;
        let k7 = self.derivative(time + step, &fifth_order)?;
        let fourth_order = combine_stage(
            state,
            step,
            &[
                (&k1, 5179.0 / 57600.0),
                (&k3, 7571.0 / 16695.0),
                (&k4, 393.0 / 640.0),
                (&k5, -92097.0 / 339200.0),
                (&k6, 187.0 / 2100.0),
                (&k7, 1.0 / 40.0),
            ],
        )?;
        let error_norm = error_norm(
            state,
            &fifth_order,
            &fourth_order,
            &self.state_absolute_tolerances,
            self.relative_tolerance,
        )?;
        Ok(TrialStep {
            endpoint: fifth_order,
            stages: [k1, k2, k3, k4, k5, k6, k7],
            error_norm,
        })
    }

    fn proposed_step(&self, request: &MeAdvanceRequest) -> Result<f64, MeIntegrationError> {
        let now = request.current().time();
        let latest = request.latest_accepted_time();
        let remaining = latest - now;
        let step = self.next_step.min(remaining);
        if !step.is_finite() || step <= 0.0 || now + step == now {
            return Err(MeIntegrationError::StepSizeUnderflow {
                method: METHOD,
                from_time: now,
                to_time: latest,
            });
        }
        if step < MIN_STEP && step < remaining {
            return Err(MeIntegrationError::StepSizeUnderflow {
                method: METHOD,
                from_time: now,
                to_time: latest,
            });
        }
        Ok(step)
    }

    fn retain_interval(
        &mut self,
        start: &MeContinuousPoint,
        step: f64,
        trial: TrialStep,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let interval = Dopri5DenseOutput::new(
            start.time(),
            step,
            start.states(),
            trial.stages.each_ref().map(Vec::as_slice),
        )
        .map_err(|error| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                error.to_string(),
            )
        })?;
        let accepted_time = interval.end_time();
        self.next_step = adapt_step(step, trial.error_norm);
        self.accepted_interval = Some(interval);
        Ok(MeStepCandidate::new(
            accepted_time,
            trial.endpoint,
            CONTINUOUS_EXTENSION_ORDER,
        ))
    }
}

impl MeIntegratorBackend for Rk45Integrator {
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        if derivatives.state_count() != point.width()
            || self.state_absolute_tolerances.len() != point.width()
        {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                "numerical setup, derivative source, and component state widths differ",
            ));
        }
        self.derivatives = Some(derivatives);
        self.next_step = self.initial_step;
        self.accepted_interval = None;
        Ok(())
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let mut step = self.proposed_step(request)?;
        loop {
            let trial =
                self.trial_step(request.current().time(), request.current().states(), step)?;
            if !trial.error_norm.is_finite() {
                return Err(MeIntegrationError::numerical(
                    METHOD,
                    MeNumericalFailure::AdvanceExhausted,
                    "the embedded error estimate is not finite",
                ));
            }
            if trial.error_norm <= 1.0 {
                return self.retain_interval(request.current(), step, trial);
            }
            if step <= MIN_STEP {
                return Err(MeIntegrationError::StepSizeUnderflow {
                    method: METHOD,
                    from_time: request.current().time(),
                    to_time: request.latest_accepted_time(),
                });
            }
            step = adapt_step(step, trial.error_norm)
                .min(request.latest_accepted_time() - request.current().time());
        }
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        let interval = self.accepted_interval.as_ref().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                "no accepted interval is available",
            )
        })?;
        if states.len() != self.state_absolute_tolerances.len() {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                "the sampler output width differs from the initialized component width",
            ));
        }
        if !accepted_interval_contains(interval.start_time(), interval.end_time(), time) {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                format!(
                    "sample time {time} lies outside [{}, {}]",
                    interval.start_time(),
                    interval.end_time()
                ),
            ));
        }
        let sampled = interval.evaluate(time).map_err(|error| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                error.to_string(),
            )
        })?;
        states.copy_from_slice(&sampled);
        Ok(())
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        if point.width() != self.state_absolute_tolerances.len() {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Reset,
                "the reset point width differs from the initialized component width",
            ));
        }
        self.accepted_interval = None;
        Ok(())
    }
}

struct TrialStep {
    endpoint: Vec<f64>,
    stages: [Vec<f64>; 7],
    error_norm: f64,
}

fn combine_stage(
    state: &[f64],
    step: f64,
    stages: &[(&[f64], f64)],
) -> Result<Vec<f64>, MeIntegrationError> {
    if stages.iter().any(|(stage, _)| stage.len() != state.len()) {
        return Err(MeIntegrationError::numerical(
            METHOD,
            MeNumericalFailure::Construction,
            "a Dormand-Prince stage width differs from the component width",
        ));
    }
    let mut combined = try_vec(state.len(), "RK45 stage")?;
    for (index, value) in state.iter().copied().enumerate() {
        let delta = stages
            .iter()
            .map(|(stage, coefficient)| coefficient * stage[index])
            .sum::<f64>();
        combined.push(value + step * delta);
    }
    Ok(combined)
}

fn error_norm(
    state: &[f64],
    high: &[f64],
    low: &[f64],
    absolute_tolerances: &[f64],
    relative_tolerance: f64,
) -> Result<f64, MeIntegrationError> {
    if high.len() != state.len()
        || low.len() != state.len()
        || absolute_tolerances.len() != state.len()
    {
        return Err(MeIntegrationError::numerical(
            METHOD,
            MeNumericalFailure::Construction,
            "the embedded estimates and tolerance vector have different widths",
        ));
    }
    Ok(state
        .iter()
        .enumerate()
        .map(|(index, value)| {
            let scale = absolute_tolerances[index]
                + relative_tolerance * value.abs().max(high[index].abs());
            (high[index] - low[index]).abs() / scale.max(f64::MIN_POSITIVE)
        })
        .fold(0.0_f64, f64::max))
}

fn adapt_step(step: f64, error_norm: f64) -> f64 {
    if error_norm <= 0.0 {
        return (step * 5.0).max(MIN_STEP);
    }
    let factor = (0.9 * error_norm.powf(-0.2)).clamp(0.2, 5.0);
    (step * factor).max(MIN_STEP)
}

fn try_vec<T>(capacity: usize, context: &'static str) -> Result<Vec<T>, MeIntegrationError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| MeIntegrationError::Allocation {
            context,
            entries: capacity,
        })?;
    Ok(values)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> MeNumericalSetup {
        MeNumericalSetup::new(1.0e-6, 1.0e-9, vec![1.0], 1, Some(0.01))
            .expect("fixture setup is checked")
    }

    #[test]
    fn zero_embedded_error_grows_by_no_more_than_the_controller_cap() {
        assert_eq!(adapt_step(0.01, 0.0), 0.05);
    }

    #[test]
    fn rejected_step_strictly_shrinks() {
        let old = 0.01;
        let adapted = adapt_step(old, 2.0);
        assert!(adapted < old);
        assert!(adapted >= MIN_STEP);
    }

    #[test]
    fn constant_stages_preserve_the_expected_endpoint() {
        let state = [3.0];
        let derivative = [2.0];
        let combined = combine_stage(
            &state,
            0.5,
            &[(&derivative, 35.0 / 384.0), (&derivative, 349.0 / 384.0)],
        )
        .expect("matching stages");
        assert!((combined[0] - 4.0).abs() < 1.0e-14);
    }

    #[test]
    fn setup_derives_one_positive_absolute_tolerance_per_state() {
        let backend = Rk45Integrator::new(setup());
        assert_eq!(backend.state_absolute_tolerances.len(), 1);
        assert!(backend.state_absolute_tolerances[0] > 0.0);
        assert_eq!(backend.initial_step.to_bits(), backend.next_step.to_bits());
    }
}
