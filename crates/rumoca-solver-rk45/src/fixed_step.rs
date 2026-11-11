//! Fixed-step explicit Runge-Kutta methods behind the solver-neutral FMI ME
//! plugin contract.
//!
//! One accepted step of length `h = min(step, latest - now)` advances
//! `x' = f(t, x)` by the method the configured order selects:
//!
//! | order | method | derivative evaluations per step |
//! |---|---|---|
//! | 1 | explicit Euler | 1 |
//! | 2 | Heun (explicit trapezoid) | 2 |
//! | 4 | classical Runge-Kutta | 4 |
//!
//! There is no error estimate, no step-size control and no rejection: the
//! step is the caller's choice, clipped only so an accepted interval never
//! crosses the request's latest accepted coordinate, on which a clipped step
//! lands exactly.
//!
//! The continuous extension is the cubic Hermite interpolant through both
//! endpoints with the step's first stage as the start slope and its last
//! stage, which every two- and four-stage method here evaluates at the end
//! time, as the end slope. It costs no evaluation. For Heun it is the method's
//! quadratic continuous extension (order 2) and for the classical method its
//! cubic one (order 3); explicit Euler has one stage, so its extension is the
//! chord (order 1). Each is the method's native extension, and the order
//! declared for a step is that extension's own.
//!
//! A component that refuses a trial point gets its typed discard back at once
//! with nothing committed: the step is fixed, so repeating it would evaluate
//! the same points, and a shorter one would be a step-size policy this method
//! does not have.
//!
//! # References
//!
//! The three tableaus and the continuous extensions of the two- and four-stage
//! methods, with the Hermite form used here: E. Hairer, S. P. Norsett and
//! G. Wanner, "Solving Ordinary Differential Equations I: Nonstiff Problems",
//! 2nd rev. ed., Springer 1993, sections II.1 (the methods of Runge, Heun and
//! Kutta) and II.6 (dense output and continuous extensions).

mod options;
#[cfg(test)]
mod tests;

pub use options::FixedStepOptionsKey;

use rumoca_solver::BackendOptionsError;
use rumoca_solver::fmi_me::{
    MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError,
    MeIntegratorBackend, MeNumericalFailure, MeNumericalSetup, MeStepCandidate,
    accepted_interval_contains,
};

use crate::me_integrator::{evaluate_derivative_in, resize_work, try_copied};

const METHOD: &str = "fixed-step";
/// Most stages any method here evaluates in one step.
const MAX_STAGES: usize = 4;

/// Fixed step and order of the explicit Runge-Kutta plugin.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FixedStepConfig {
    /// The fixed internal step `h` in seconds.
    pub step: f64,
    /// The method's order: 1 (explicit Euler), 2 (Heun) or 4 (classical
    /// Runge-Kutta). It is also the derivative evaluations spent per step.
    pub order: u32,
}

impl Default for FixedStepConfig {
    fn default() -> Self {
        Self {
            step: 1.0e-3,
            order: 4,
        }
    }
}

/// The explicit method one configured order selects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Method {
    Euler,
    Heun,
    Classical,
}

impl Method {
    const fn from_order(order: u32) -> Option<Self> {
        match order {
            1 => Some(Self::Euler),
            2 => Some(Self::Heun),
            4 => Some(Self::Classical),
            _ => None,
        }
    }

    /// Local accuracy order of the method's native continuous extension.
    const fn continuous_extension_order(self) -> u32 {
        match self {
            Self::Euler => 1,
            Self::Heun => 2,
            Self::Classical => 3,
        }
    }

    /// Index of the stage evaluated at the end time, when the method has one.
    const fn end_stage(self) -> Option<usize> {
        match self {
            Self::Euler => None,
            Self::Heun => Some(1),
            Self::Classical => Some(3),
        }
    }
}

/// Build the fixed-step explicit plugin accepted by the common FMI ME host.
///
/// `config` is validated here as well, so a value that never passed
/// [`FixedStepConfig::validate_request`] is the same typed failure rather than
/// a silently substituted method.
pub fn fixed_step_integrator(
    setup: MeNumericalSetup,
    config: FixedStepConfig,
) -> Result<Box<dyn MeIntegratorBackend + 'static>, BackendOptionsError> {
    let config = config.validate_request()?;
    let method = Method::from_order(config.order).ok_or_else(options::invalid_order)?;
    Ok(Box::new(FixedStepIntegrator::new(
        setup.state_nominals().len(),
        config.step,
        method,
    )))
}

struct FixedStepIntegrator {
    width: usize,
    step: f64,
    method: Method,
    derivatives: Option<MeDerivativeHandle>,
    work: Work,
    accepted_interval: Option<HermiteInterval>,
}

#[derive(Default)]
struct Work {
    /// Stage derivatives `k_1..k_s` of the current step.
    slopes: [Vec<f64>; MAX_STAGES],
    /// The stage state a derivative is evaluated at.
    stage: Vec<f64>,
    /// The step's endpoint.
    end: Vec<f64>,
}

/// One accepted interval and what its continuous extension reads.
#[derive(Default)]
struct HermiteInterval {
    start_time: f64,
    end_time: f64,
    /// The step length the slopes are scaled by.
    step: f64,
    start: Vec<f64>,
    end: Vec<f64>,
    start_slope: Vec<f64>,
    /// Empty for the one-stage method, whose extension is the chord.
    end_slope: Vec<f64>,
}

impl HermiteInterval {
    fn evaluate_into(&self, time: f64, states: &mut [f64]) {
        let span = self.end_time - self.start_time;
        let theta = if span > 0.0 {
            ((time - self.start_time) / span).clamp(0.0, 1.0)
        } else {
            1.0
        };
        let chord = states.iter_mut().zip(&self.start).zip(&self.end);
        if self.end_slope.is_empty() {
            for ((out, start), end) in chord {
                *out = (1.0 - theta) * start + theta * end;
            }
            return;
        }
        let slopes = self.start_slope.iter().zip(&self.end_slope);
        for (((out, start), end), (start_slope, end_slope)) in chord.zip(slopes) {
            let curvature = (1.0 - 2.0 * theta) * (end - start)
                + (theta - 1.0) * self.step * start_slope
                + theta * self.step * end_slope;
            *out = (1.0 - theta) * start + theta * end + theta * (theta - 1.0) * curvature;
        }
    }
}

impl FixedStepIntegrator {
    fn new(width: usize, step: f64, method: Method) -> Self {
        Self {
            width,
            step,
            method,
            derivatives: None,
            work: Work::default(),
            accepted_interval: None,
        }
    }

    /// One step of length `step` ending at `end_time`; the endpoint lands in
    /// `work.end` and the stage derivatives stay in `work.slopes`.
    fn trial_step(
        &mut self,
        time: f64,
        state: &[f64],
        step: f64,
        end_time: f64,
    ) -> Result<(), MeIntegrationError> {
        let derivatives = self.derivatives.as_ref().ok_or_else(missing_handle)?;
        let Work {
            slopes: k,
            stage,
            end,
        } = &mut self.work;
        evaluate(derivatives, time, state, &mut k[0])?;
        match self.method {
            Method::Euler => combine_into(state, step, &[(&k[0], 1.0)], end),
            Method::Heun => {
                combine_into(state, step, &[(&k[0], 1.0)], stage);
                evaluate(derivatives, end_time, stage, &mut k[1])?;
                combine_into(state, step, &[(&k[0], 0.5), (&k[1], 0.5)], end);
            }
            Method::Classical => {
                let midpoint = time + 0.5 * step;
                combine_into(state, step, &[(&k[0], 0.5)], stage);
                evaluate(derivatives, midpoint, stage, &mut k[1])?;
                combine_into(state, step, &[(&k[1], 0.5)], stage);
                evaluate(derivatives, midpoint, stage, &mut k[2])?;
                combine_into(state, step, &[(&k[2], 1.0)], stage);
                evaluate(derivatives, end_time, stage, &mut k[3])?;
                let weights = [
                    (k[0].as_slice(), 1.0 / 6.0),
                    (k[1].as_slice(), 1.0 / 3.0),
                    (k[2].as_slice(), 1.0 / 3.0),
                    (k[3].as_slice(), 1.0 / 6.0),
                ];
                combine_into(state, step, &weights, end);
            }
        }
        if end.iter().any(|value| !value.is_finite()) {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::AdvanceExhausted,
                "the explicit step produced a non-finite state",
            ));
        }
        Ok(())
    }

    /// Retain the step just taken as the accepted interval, reusing the
    /// previous interval's buffers.
    fn retain_interval(&mut self, start: &MeContinuousPoint, step: f64, end_time: f64) {
        let mut interval = self.accepted_interval.take().unwrap_or_default();
        interval.start_time = start.time();
        interval.end_time = end_time;
        interval.step = step;
        overwrite(&mut interval.start, start.states());
        overwrite(&mut interval.end, &self.work.end);
        interval.end_slope.clear();
        if let Some(end_stage) = self.method.end_stage() {
            overwrite(&mut interval.start_slope, &self.work.slopes[0]);
            overwrite(&mut interval.end_slope, &self.work.slopes[end_stage]);
        }
        self.accepted_interval = Some(interval);
    }
}

impl MeIntegratorBackend for FixedStepIntegrator {
    fn continuous_extension_order(&self) -> Option<u32> {
        Some(self.method.continuous_extension_order())
    }

    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        if derivatives.state_count() != point.width() || self.width != point.width() {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                "numerical setup, derivative source, and component state widths differ",
            ));
        }
        let width = self.width;
        for slope in &mut self.work.slopes {
            resize_work(slope, width, "fixed-step derivative workspace")?;
        }
        resize_work(&mut self.work.stage, width, "fixed-step stage workspace")?;
        resize_work(&mut self.work.end, width, "fixed-step endpoint workspace")?;
        self.derivatives = Some(derivatives);
        self.accepted_interval = None;
        Ok(())
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let start = request.current();
        let now = start.time();
        let latest = request.latest_accepted_time();
        let (step, end_time) = bounded_step(now, latest, request.max_step_duration(), self.step)
            .ok_or(MeIntegrationError::StepSizeUnderflow {
                method: METHOD,
                from_time: now,
                to_time: latest,
            })?;
        // The step is fixed: a refused trial point would be refused again by
        // the same step, so the typed failure is returned at once with the
        // accepted interval untouched.
        self.trial_step(now, start.states(), step, end_time)?;
        let endpoint = try_copied(&self.work.end, "fixed-step accepted endpoint")?;
        self.retain_interval(start, step, end_time);
        Ok(MeStepCandidate::new(
            end_time,
            endpoint,
            self.method.continuous_extension_order(),
        ))
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        let interval = self.accepted_interval.as_ref().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                "no accepted interval is available",
            )
        })?;
        if states.len() != self.width {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                "the sampler output width differs from the initialized component width",
            ));
        }
        if !accepted_interval_contains(interval.start_time, interval.end_time, time) {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                format!(
                    "sample time {time} lies outside [{}, {}]",
                    interval.start_time, interval.end_time
                ),
            ));
        }
        interval.evaluate_into(time, states);
        Ok(())
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        if point.width() != self.width {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Reset,
                "the reset point width differs from the initialized component width",
            ));
        }
        // No stage, slope or step-size history survives a step, so the only
        // thing to forget is the interval the host just truncated.
        self.accepted_interval = None;
        Ok(())
    }
}

fn missing_handle() -> MeIntegrationError {
    MeIntegrationError::numerical(
        METHOD,
        MeNumericalFailure::Construction,
        "the host has not initialized the derivative capability",
    )
}

fn evaluate(
    derivatives: &MeDerivativeHandle,
    time: f64,
    states: &[f64],
    out: &mut Vec<f64>,
) -> Result<(), MeIntegrationError> {
    evaluate_derivative_in(
        derivatives,
        time,
        states,
        out,
        "fixed-step derivative workspace",
    )
}

/// Overwrite a retained buffer, keeping its allocation.
fn overwrite(buffer: &mut Vec<f64>, source: &[f64]) {
    buffer.clear();
    buffer.extend_from_slice(source);
}

/// `out = state + step * sum(weight * stage)`.
fn combine_into(state: &[f64], step: f64, stages: &[(&[f64], f64)], out: &mut [f64]) {
    for (index, (out, value)) in out.iter_mut().zip(state).enumerate() {
        let delta: f64 = stages
            .iter()
            .map(|(stage, weight)| weight * stage[index])
            .sum();
        *out = value + step * delta;
    }
}

/// The step length and the coordinate it ends at: `step` clipped to
/// `max_step` and to `latest - now`, where a step clipped to the remaining
/// interval ends exactly at `latest`. `None` when the length is not positive
/// and finite or does not advance the coordinate.
fn bounded_step(now: f64, latest: f64, max_step: Option<f64>, step: f64) -> Option<(f64, f64)> {
    let remaining = latest - now;
    let unclipped = max_step.map_or(step, |max| step.min(max));
    let (length, end_time) = if remaining <= unclipped {
        (remaining, latest)
    } else {
        (unclipped, now + unclipped)
    };
    (length.is_finite() && length > 0.0 && end_time > now).then_some((length, end_time))
}
