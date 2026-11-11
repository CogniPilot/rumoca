//! Opt-in Cartesian semi-implicit Euler. FMI lifecycle and observation policy
//! remain in the common host. This profile does not implement implicit damping.

mod observation;
mod quaternion;
pub use observation::{CartesianWorkCounts, CartesianWorkObserver};
pub use quaternion::UnitBodyQuaternion;

use rumoca_solver::fmi_me::{
    MeAdvanceRequest, MeCartesianPartition, MeContinuousPoint, MeDerivativeHandle,
    MeIntegrationError, MeIntegratorBackend, MeNumericalFailure, MeNumericalSetup, MeStepCandidate,
    accepted_interval_contains,
};

const METHOD: &str = "Cartesian semi-implicit Euler";
const STEP: f64 = 0.005;

/// Explicit opt-in factory; the ordinary numerical dispatcher is unchanged.
#[must_use]
pub fn model_exchange_integrator(setup: MeNumericalSetup) -> Box<dyn MeIntegratorBackend> {
    integrator(setup, STEP, None)
}

/// An explicit fixed step for manufactured convergence controls and callers
/// requiring a finer Cartesian grid. The matching default remains 5 ms.
pub fn model_exchange_integrator_with_step(
    setup: MeNumericalSetup,
    step: f64,
) -> Result<Box<dyn MeIntegratorBackend>, MeIntegrationError> {
    validate_step(step)?;
    Ok(integrator(setup, step, None))
}

fn validate_step(step: f64) -> Result<(), MeIntegrationError> {
    if !step.is_finite() || step <= 0.0 || step > STEP {
        return Err(failure(
            MeNumericalFailure::Construction,
            "step must be finite and in (0, 0.005]",
        ));
    }
    Ok(())
}

fn integrator(
    setup: MeNumericalSetup,
    step: f64,
    observation: Option<CartesianWorkObserver>,
) -> Box<dyn MeIntegratorBackend> {
    Box::new(CartesianEuler {
        width: setup.state_nominals().len(),
        step,
        derivatives: None,
        partition: None,
        interval: None,
        observation,
    })
}

/// The same plugin with optional request counters, never an extra evaluation.
pub fn model_exchange_integrator_observed(
    setup: MeNumericalSetup,
    step: f64,
) -> Result<(Box<dyn MeIntegratorBackend>, CartesianWorkObserver), MeIntegrationError> {
    validate_step(step)?;
    let observation = CartesianWorkObserver::default();
    Ok((
        integrator(setup, step, Some(observation.clone())),
        observation,
    ))
}

struct CartesianEuler {
    width: usize,
    step: f64,
    derivatives: Option<MeDerivativeHandle>,
    partition: Option<MeCartesianPartition>,
    interval: Option<Interval>,
    observation: Option<CartesianWorkObserver>,
}

struct Interval {
    start: f64,
    end: f64,
    before: Vec<f64>,
    after: Vec<f64>,
}

fn failure(category: MeNumericalFailure, message: &str) -> MeIntegrationError {
    MeIntegrationError::numerical(METHOD, category, message)
}

impl MeIntegratorBackend for CartesianEuler {
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        self.derivatives = None;
        self.partition = None;
        self.interval = None;
        if point.width() != self.width || derivatives.state_count() != self.width {
            return Err(failure(
                MeNumericalFailure::Construction,
                "inconsistent state extent",
            ));
        }
        self.partition = Some(derivatives.cartesian_partition()?);
        self.derivatives = Some(derivatives);
        Ok(())
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let current = request.current();
        let step = self
            .step
            .min(request.max_step_duration().unwrap_or(self.step))
            .min(request.latest_accepted_time() - current.time());
        let end = current.time() + step;
        if !step.is_finite() || step <= 0.0 || end <= current.time() {
            return Err(MeIntegrationError::StepSizeUnderflow {
                method: METHOD,
                from_time: current.time(),
                to_time: request.latest_accepted_time(),
            });
        }
        let handle = self.derivatives.as_ref().ok_or_else(|| {
            failure(
                MeNumericalFailure::Construction,
                "integrator is not initialized",
            )
        })?;
        let partition = self.partition.as_ref().ok_or_else(|| {
            failure(
                MeNumericalFailure::Construction,
                "no retained Cartesian partition",
            )
        })?;
        // One component derivative request, no directional action/Jacobian.
        if let Some(observation) = &self.observation {
            observation.derivative_request();
        }
        let rates = handle.derivatives(current.time(), current.states())?;
        let mut after = current.states().to_vec();
        for (_, velocity) in partition.pairs() {
            after[velocity] = current.states()[velocity] + step * rates[velocity];
        }
        for (position, velocity) in partition.pairs() {
            after[position] = current.states()[position] + step * after[velocity];
        }
        if after.iter().any(|value| !value.is_finite()) {
            return Err(failure(
                MeNumericalFailure::AdvanceExhausted,
                "non-finite Cartesian step",
            ));
        }
        self.interval = Some(Interval {
            start: current.time(),
            end,
            before: current.states().to_vec(),
            after: after.clone(),
        });
        if let Some(observation) = &self.observation {
            observation.candidate();
        }
        Ok(MeStepCandidate::new(end, after, 1))
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        let interval = self
            .interval
            .as_ref()
            .ok_or_else(|| failure(MeNumericalFailure::Interpolation, "no accepted interval"))?;
        if states.len() != self.width
            || !time.is_finite()
            || !accepted_interval_contains(interval.start, interval.end, time)
        {
            return Err(failure(
                MeNumericalFailure::Interpolation,
                "invalid sample coordinate or extent",
            ));
        }
        let fraction = ((time - interval.start) / (interval.end - interval.start)).clamp(0.0, 1.0);
        if time == interval.start {
            states.copy_from_slice(&interval.before);
        } else if time == interval.end {
            states.copy_from_slice(&interval.after);
        } else {
            for ((out, before), after) in
                states.iter_mut().zip(&interval.before).zip(&interval.after)
            {
                *out = before + fraction * (after - before);
            }
        }
        Ok(())
    }

    fn continuous_extension_order(&self) -> Option<u32> {
        Some(1)
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        self.interval = None;
        if point.width() != self.width {
            return Err(failure(
                MeNumericalFailure::Reset,
                "reset state extent differs",
            ));
        }
        Ok(())
    }
}
