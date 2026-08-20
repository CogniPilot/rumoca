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
    work: Rk45Work,
    fsal_time: Option<f64>,
}

struct Rk45Work {
    derivatives: [Vec<f64>; 7],
    states: [Vec<f64>; 7],
}

impl Default for Rk45Work {
    fn default() -> Self {
        Self {
            derivatives: std::array::from_fn(|_| Vec::new()),
            states: std::array::from_fn(|_| Vec::new()),
        }
    }
}

impl Rk45Work {
    fn resize(&mut self, width: usize) -> Result<(), MeIntegrationError> {
        for derivative in &mut self.derivatives {
            resize_work(derivative, width, "RK45 derivative workspace")?;
        }
        for state in &mut self.states {
            resize_work(state, width, "RK45 state workspace")?;
        }
        Ok(())
    }
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
            work: Rk45Work::default(),
            fsal_time: None,
        }
    }

    fn trial_step(
        &mut self,
        time: f64,
        state: &[f64],
        step: f64,
    ) -> Result<f64, MeIntegrationError> {
        let derivatives = self.derivatives.as_ref().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                "the host has not initialized the derivative capability",
            )
        })?;
        let Rk45Work {
            derivatives: k,
            states: y,
        } = &mut self.work;

        if self
            .fsal_time
            .take()
            .is_some_and(|cached| cached.to_bits() == time.to_bits())
        {
            k.swap(0, 6);
        } else {
            evaluate_derivative(derivatives, time, state, &mut k[0])?;
        }
        combine_stage_into(state, step, &[(&k[0], 1.0 / 5.0)], &mut y[0])?;
        evaluate_derivative(derivatives, time + step * (1.0 / 5.0), &y[0], &mut k[1])?;

        combine_stage_into(
            state,
            step,
            &[(&k[0], 3.0 / 40.0), (&k[1], 9.0 / 40.0)],
            &mut y[1],
        )?;
        evaluate_derivative(derivatives, time + step * (3.0 / 10.0), &y[1], &mut k[2])?;

        combine_stage_into(
            state,
            step,
            &[
                (&k[0], 44.0 / 45.0),
                (&k[1], -56.0 / 15.0),
                (&k[2], 32.0 / 9.0),
            ],
            &mut y[2],
        )?;
        evaluate_derivative(derivatives, time + step * (4.0 / 5.0), &y[2], &mut k[3])?;

        combine_stage_into(
            state,
            step,
            &[
                (&k[0], 19372.0 / 6561.0),
                (&k[1], -25360.0 / 2187.0),
                (&k[2], 64448.0 / 6561.0),
                (&k[3], -212.0 / 729.0),
            ],
            &mut y[3],
        )?;
        evaluate_derivative(derivatives, time + step * (8.0 / 9.0), &y[3], &mut k[4])?;

        combine_stage_into(
            state,
            step,
            &[
                (&k[0], 9017.0 / 3168.0),
                (&k[1], -355.0 / 33.0),
                (&k[2], 46732.0 / 5247.0),
                (&k[3], 49.0 / 176.0),
                (&k[4], -5103.0 / 18656.0),
            ],
            &mut y[4],
        )?;
        evaluate_derivative(derivatives, time + step, &y[4], &mut k[5])?;

        combine_stage_into(
            state,
            step,
            &[
                (&k[0], 35.0 / 384.0),
                (&k[2], 500.0 / 1113.0),
                (&k[3], 125.0 / 192.0),
                (&k[4], -2187.0 / 6784.0),
                (&k[5], 11.0 / 84.0),
            ],
            &mut y[5],
        )?;
        evaluate_derivative(derivatives, time + step, &y[5], &mut k[6])?;
        combine_stage_into(
            state,
            step,
            &[
                (&k[0], 5179.0 / 57600.0),
                (&k[2], 7571.0 / 16695.0),
                (&k[3], 393.0 / 640.0),
                (&k[4], -92097.0 / 339200.0),
                (&k[5], 187.0 / 2100.0),
                (&k[6], 1.0 / 40.0),
            ],
            &mut y[6],
        )?;
        error_norm(
            state,
            &y[5],
            &y[6],
            &self.state_absolute_tolerances,
            self.relative_tolerance,
        )
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
        error_norm: f64,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let interval = Dopri5DenseOutput::new(
            start.time(),
            step,
            start.states(),
            self.work.derivatives.each_ref().map(Vec::as_slice),
        )
        .map_err(|error| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                error.to_string(),
            )
        })?;
        let accepted_time = interval.end_time();
        let endpoint = try_copied(&self.work.states[5], "RK45 accepted endpoint")?;
        self.next_step = adapt_step(step, error_norm);
        self.fsal_time = Some(accepted_time);
        self.accepted_interval = Some(interval);
        Ok(MeStepCandidate::new(
            accepted_time,
            endpoint,
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
        self.work.resize(point.width())?;
        self.derivatives = Some(derivatives);
        self.next_step = self.initial_step;
        self.accepted_interval = None;
        self.fsal_time = None;
        Ok(())
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let mut step = self.proposed_step(request)?;
        loop {
            let error_norm =
                self.trial_step(request.current().time(), request.current().states(), step)?;
            if !error_norm.is_finite() {
                return Err(MeIntegrationError::numerical(
                    METHOD,
                    MeNumericalFailure::AdvanceExhausted,
                    "the embedded error estimate is not finite",
                ));
            }
            if error_norm <= 1.0 {
                return self.retain_interval(request.current(), step, error_norm);
            }
            self.fsal_time = None;
            if step <= MIN_STEP {
                return Err(MeIntegrationError::StepSizeUnderflow {
                    method: METHOD,
                    from_time: request.current().time(),
                    to_time: request.latest_accepted_time(),
                });
            }
            step = adapt_step(step, error_norm)
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
        interval.evaluate_into(time, states).map_err(|error| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                error.to_string(),
            )
        })
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
        self.fsal_time = None;
        Ok(())
    }
}

#[cfg(test)]
fn combine_stage(
    state: &[f64],
    step: f64,
    stages: &[(&[f64], f64)],
) -> Result<Vec<f64>, MeIntegrationError> {
    let mut combined = Vec::new();
    combine_stage_into(state, step, stages, &mut combined)?;
    Ok(combined)
}

fn combine_stage_into(
    state: &[f64],
    step: f64,
    stages: &[(&[f64], f64)],
    combined: &mut Vec<f64>,
) -> Result<(), MeIntegrationError> {
    if stages.iter().any(|(stage, _)| stage.len() != state.len()) {
        return Err(MeIntegrationError::numerical(
            METHOD,
            MeNumericalFailure::Construction,
            "a Dormand-Prince stage width differs from the component width",
        ));
    }
    resize_work(combined, state.len(), "RK45 stage")?;
    for (index, value) in state.iter().copied().enumerate() {
        let delta = stages
            .iter()
            .map(|(stage, coefficient)| coefficient * stage[index])
            .sum::<f64>();
        combined[index] = value + step * delta;
    }
    Ok(())
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

fn resize_work<T: Clone + Default>(
    values: &mut Vec<T>,
    len: usize,
    context: &'static str,
) -> Result<(), MeIntegrationError> {
    if len > values.capacity() {
        values.try_reserve_exact(len - values.len()).map_err(|_| {
            MeIntegrationError::Allocation {
                context,
                entries: len,
            }
        })?;
    }
    values.resize(len, T::default());
    Ok(())
}

fn try_copied<T: Copy>(values: &[T], context: &'static str) -> Result<Vec<T>, MeIntegrationError> {
    let mut copied = try_vec(values.len(), context)?;
    copied.extend_from_slice(values);
    Ok(copied)
}

fn evaluate_derivative(
    derivatives: &MeDerivativeHandle,
    time: f64,
    states: &[f64],
    out: &mut Vec<f64>,
) -> Result<(), MeIntegrationError> {
    resize_work(out, derivatives.state_count(), "RK45 derivative workspace")?;
    derivatives.derivatives_into(time, states, out);
    if derivatives.has_failed() {
        return Err(MeIntegrationError::DerivativeRefused);
    }
    Ok(())
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
