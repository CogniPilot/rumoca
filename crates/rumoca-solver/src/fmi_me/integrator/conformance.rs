//! The solver-neutral plugin conformance suite (SPEC_0044 §6, ME-INT-001).
//!
//! ME-INT-001 requires an *unrelated* minimal integrator to be admitted through
//! exactly the same checked contract as the production backends, and requires a
//! manufactured-solution convergence suite that consumes a plugin's **declared**
//! order and rejects a deliberately degraded sampler.
//!
//! [`HermiteStepIntegrator`] is that unrelated solver. It shares no type with
//! Diffsol or RK45: it is a classical four-stage Runge-Kutta step with a cubic
//! Hermite continuous extension, written against nothing but
//! [`MeIntegratorBackend`] and the retained [`MeDerivativeHandle`]. Because it
//! is test-only it can never become a production path, and because it knows no
//! FMI operation at all it is direct evidence that the boundary carries no
//! lifecycle, event, trace, or component policy.
//!
//! It exercises the **exact** retained-handle contract: it stores the handle a
//! host issues at `initialize` and evaluates through that stored handle in
//! `advance` and `truncate_reset`. The same plugin therefore drives a
//! manufactured solution (through a closure-backed controller) and a real FMI
//! component (through the session's own controller) without any change, and
//! [`SamplerQuality::ProbesInactiveCapability`] proves that a request made from
//! inside `sample` is refused rather than served at a stale coordinate.

use std::rc::Rc;

use super::{
    MeAdvanceRequest, MeContinuousPoint, MeDerivativeController, MeDerivativeHandle,
    MeIntegrationError, MeIntegratorBackend, MeNumericalFailure, MeStepCandidate, MeStepProposal,
    sample_complete,
};
use crate::fmi_me::MeContinuousStateDomain;

use super::DerivativeClosure;

/// The local accuracy order of a cubic Hermite continuous extension.
const HERMITE_ORDER: u32 = 4;

/// The local accuracy order of straight-line interpolation, used only by the
/// deliberately degraded control.
const DEGRADED_SAMPLER_ORDER: u32 = 2;

/// How a plugin's continuous extension samples its accepted interval.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SamplerQuality {
    /// The plugin's real cubic Hermite extension.
    Native,
    /// A deliberately degraded straight-line extension that still declares the
    /// native order. The convergence suite MUST reject it.
    DegradedLinear,
    /// The real extension, plus one illegal component request issued from
    /// inside `sample`. The retained handle MUST refuse it.
    ProbesInactiveCapability,
}

/// One accepted step's endpoints and endpoint derivatives.
struct StepRecord {
    t0: f64,
    y0: Vec<f64>,
    f0: Vec<f64>,
    t1: f64,
    y1: Vec<f64>,
    f1: Vec<f64>,
}

/// An integrator unrelated to every production backend.
pub(crate) struct HermiteStepIntegrator {
    width: usize,
    quality: SamplerQuality,
    step: Option<StepRecord>,
    /// The retained opaque capability. A persistent method would hand this to
    /// its own equations; this plugin simply keeps it across accepted steps,
    /// which is the property the lent `&dyn` form could not express.
    derivatives: Option<MeDerivativeHandle>,
}

impl HermiteStepIntegrator {
    pub(crate) fn new(width: usize, quality: SamplerQuality) -> Self {
        Self {
            width,
            quality,
            step: None,
            derivatives: None,
        }
    }

    fn evaluate(&self, time: f64, states: &[f64]) -> Result<Vec<f64>, MeIntegrationError> {
        let handle = self.derivatives.as_ref().ok_or_else(|| {
            MeIntegrationError::numerical(
                "hermite-rk4",
                MeNumericalFailure::Construction,
                "the plugin was never issued a derivative handle",
            )
        })?;
        let values = handle.derivatives(time, states)?;
        if values.len() != self.width {
            return Err(MeIntegrationError::numerical(
                "hermite-rk4",
                MeNumericalFailure::Construction,
                format!(
                    "the retained capability returned {} values for a width of {}",
                    values.len(),
                    self.width
                ),
            ));
        }
        Ok(values)
    }

    fn settle_at(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        if point.width() != self.width {
            return Err(MeIntegrationError::numerical(
                "hermite-rk4",
                MeNumericalFailure::Reset,
                format!(
                    "a point of width {} cannot restart a plugin of width {}",
                    point.width(),
                    self.width
                ),
            ));
        }
        let derivative = self.evaluate(point.time(), point.states())?;
        self.step = Some(StepRecord {
            t0: point.time(),
            y0: point.states().to_vec(),
            f0: derivative.clone(),
            t1: point.time(),
            y1: point.states().to_vec(),
            f1: derivative,
        });
        Ok(())
    }
}

impl MeIntegratorBackend for HermiteStepIntegrator {
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        self.derivatives = Some(derivatives);
        self.settle_at(point)
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let t0 = request.current().time();
        let y0 = request.current().states();
        if y0.len() != self.width {
            return Err(MeIntegrationError::numerical(
                "hermite-rk4",
                MeNumericalFailure::Construction,
                "the host issued a request of the wrong width",
            ));
        }
        let t1 = request.latest_accepted_time();
        let h = t1 - t0;

        let f0 = self.evaluate(t0, y0)?;
        let k2 = self.evaluate(t0 + 0.5 * h, &axpy(y0, &f0, 0.5 * h))?;
        let k3 = self.evaluate(t0 + 0.5 * h, &axpy(y0, &k2, 0.5 * h))?;
        let k4 = self.evaluate(t1, &axpy(y0, &k3, h))?;
        let mut y1 = y0.to_vec();
        for (index, slot) in y1.iter_mut().enumerate() {
            *slot += h / 6.0 * (f0[index] + 2.0 * k2[index] + 2.0 * k3[index] + k4[index]);
        }
        let f1 = self.evaluate(t1, &y1)?;

        // The endpoint is exactly the request's reachable bound, which is the
        // least of its host-issued coordinates, so the host's canonicalization
        // is the identity and the interval recorded here is the one the host
        // will sample.
        self.step = Some(StepRecord {
            t0,
            y0: y0.to_vec(),
            f0,
            t1,
            y1: y1.clone(),
            f1,
        });
        Ok(MeStepCandidate::new(t1, y1, HERMITE_ORDER))
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        let Some(step) = &self.step else {
            return Err(MeIntegrationError::numerical(
                "hermite-rk4",
                MeNumericalFailure::Interpolation,
                "no accepted interval has been established",
            ));
        };
        if states.len() != self.width {
            return Err(MeIntegrationError::numerical(
                "hermite-rk4",
                MeNumericalFailure::Interpolation,
                "the host buffer does not match the component width",
            ));
        }
        if !time.is_finite() || time < step.t0 || time > step.t1 {
            return Err(MeIntegrationError::numerical(
                "hermite-rk4",
                MeNumericalFailure::Interpolation,
                format!("t={time} lies outside the accepted interval"),
            ));
        }
        if self.quality == SamplerQuality::ProbesInactiveCapability {
            // A native continuous extension reads stored stage values. Asking
            // the component here is exactly the misuse the activation state
            // exists to refuse.
            self.evaluate(time, &step.y0)?;
        }
        let h = step.t1 - step.t0;
        if h <= 0.0 {
            states.copy_from_slice(&step.y1);
            return Ok(());
        }
        let theta = (time - step.t0) / h;
        for (index, slot) in states.iter_mut().enumerate() {
            *slot = match self.quality {
                SamplerQuality::Native | SamplerQuality::ProbesInactiveCapability => hermite(
                    theta,
                    h,
                    step.y0[index],
                    step.f0[index],
                    step.y1[index],
                    step.f1[index],
                ),
                SamplerQuality::DegradedLinear => {
                    step.y0[index] + theta * (step.y1[index] - step.y0[index])
                }
            };
        }
        Ok(())
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        self.settle_at(point)
    }
}

fn axpy(base: &[f64], direction: &[f64], scale: f64) -> Vec<f64> {
    base.iter()
        .zip(direction)
        .map(|(value, slope)| value + scale * slope)
        .collect()
}

/// The standard cubic Hermite basis over one accepted interval.
fn hermite(theta: f64, h: f64, y0: f64, f0: f64, y1: f64, f1: f64) -> f64 {
    let t2 = theta * theta;
    let t3 = t2 * theta;
    (2.0 * t3 - 3.0 * t2 + 1.0) * y0
        + (t3 - 2.0 * t2 + theta) * h * f0
        + (-2.0 * t3 + 3.0 * t2) * y1
        + (t3 - t2) * h * f1
}

/// One manufactured solution: a closed-form trajectory and its derivative.
pub(crate) struct ManufacturedSolution {
    pub(crate) name: &'static str,
    pub(crate) exact: fn(f64) -> f64,
    pub(crate) derivative: fn(f64) -> f64,
}

/// The largest continuous-extension error over `steps` accepted steps of width
/// `width_per_step`, measured strictly inside each interval.
///
/// The measurement is solver-neutral: it drives the plugin only through the
/// checked contract, and it opens the activation window for exactly the calls
/// the common host opens it for, so the fixture cannot accidentally give the
/// plugin a capability the production host withholds.
pub(crate) fn max_sampler_error(
    quality: SamplerQuality,
    solution: &ManufacturedSolution,
    start: f64,
    width_per_step: f64,
    steps: usize,
) -> Result<f64, MeIntegrationError> {
    let derivative = solution.derivative;
    let controller = MeDerivativeController::over_closure(
        1,
        Rc::new(move |time, _states: &[f64]| vec![derivative(time)]),
    );
    let mut plugin = HermiteStepIntegrator::new(1, quality);
    let mut time = start;
    let mut state = vec![(solution.exact)(start)];
    {
        let _window = controller.activate();
        plugin.initialize(
            &MeContinuousPoint::new(
                time,
                state.clone(),
                MeContinuousStateDomain::verification_fixture(1),
            )?,
            controller.issue_handle(),
        )?;
    }

    let mut worst: f64 = 0.0;
    let mut sampled = vec![0.0_f64];
    for _ in 0..steps {
        let request = MeAdvanceRequest::new(
            MeContinuousPoint::new(
                time,
                state.clone(),
                MeContinuousStateDomain::verification_fixture(1),
            )?,
            None,
            time + width_per_step,
            None,
            None,
        )?;
        let candidate = {
            let _window = controller.activate();
            plugin.advance(&request)?
        };
        // The suite drives the *whole* checked path, binding the candidate to
        // the actual request exactly as the session does, so the order it
        // consumes is the declared one that survived host validation.
        let step = MeStepProposal::bind(request, candidate)?;
        let width = step.accepted().time() - step.previous().time();
        for tenth in 1..10 {
            let theta = f64::from(tenth) / 10.0;
            let coordinate = step.previous().time() + theta * width;
            // Deliberately outside the window: SPEC_0044 §6's ruling makes the
            // capability inactive during `sample`.
            sample_complete(&plugin, coordinate, &mut sampled)?;
            worst = worst.max((sampled[0] - (solution.exact)(coordinate)).abs());
        }
        time = step.accepted().time();
        state = step.accepted().states().to_vec();
    }
    Ok(worst)
}

/// The order the sampler actually attains between two refinements.
pub(crate) fn observed_order(coarse_error: f64, fine_error: f64) -> f64 {
    if fine_error <= 0.0 || coarse_error <= 0.0 {
        return f64::INFINITY;
    }
    (coarse_error / fine_error).log2()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmi_me::integrator::{detached_handle, time_only::TimeOnlyIntegrator};

    /// Curved, with an inflection point and equal endpoint derivatives over
    /// each unit interval: a straight-line sampler agrees at both endpoints
    /// and is badly wrong in between, which is exactly the case endpoint
    /// checks cannot catch and convergence evidence must.
    const SINE: ManufacturedSolution = ManufacturedSolution {
        name: "sin(2*pi*t)/(2*pi)",
        exact: |t| (std::f64::consts::TAU * t).sin() / std::f64::consts::TAU,
        derivative: |t| (std::f64::consts::TAU * t).cos(),
    };

    /// A curved polynomial with two inflection points, one degree above what a
    /// cubic extension can reproduce exactly.
    const QUARTIC: ManufacturedSolution = ManufacturedSolution {
        name: "t^4 - 2*t^3 + 0.5*t^2",
        exact: |t| t * t * t * t - 2.0 * t * t * t + 0.5 * t * t,
        derivative: |t| 4.0 * t * t * t - 6.0 * t * t + t,
    };

    /// A genuine cubic: the Hermite extension reproduces it exactly, so it is
    /// an exactness witness rather than a convergence-rate witness.
    const CUBIC: ManufacturedSolution = ManufacturedSolution {
        name: "t^3 - 1.5*t^2",
        exact: |t| t * t * t - 1.5 * t * t,
        derivative: |t| 3.0 * t * t - 3.0 * t,
    };

    fn unit_rate_controller() -> MeDerivativeController {
        MeDerivativeController::over_closure(1, Rc::new(|_t, _y| vec![1.0]))
    }

    fn order_over(quality: SamplerQuality, solution: &ManufacturedSolution) -> f64 {
        let coarse = max_sampler_error(quality, solution, 0.0, 0.2, 5).expect("coarse run");
        let fine = max_sampler_error(quality, solution, 0.0, 0.1, 10).expect("fine run");
        observed_order(coarse, fine)
    }

    #[test]
    fn an_unrelated_plugin_is_admitted_through_exactly_the_checked_contract() {
        let controller = unit_rate_controller();
        let mut plugin = HermiteStepIntegrator::new(1, SamplerQuality::Native);
        let start = MeContinuousPoint::new(
            0.0,
            vec![0.0],
            MeContinuousStateDomain::verification_fixture(1),
        )
        .expect("checked point");
        {
            let _window = controller.activate();
            plugin
                .initialize(&start, controller.issue_handle())
                .expect("initialize");
        }
        let request =
            MeAdvanceRequest::new(start, Some(0.5), 1.0, None, None).expect("checked request");
        let candidate = {
            let _window = controller.activate();
            plugin.advance(&request).expect("one accepted step")
        };
        let step = MeStepProposal::bind(request, candidate).expect("the candidate binds");
        assert!((step.accepted().time() - 0.5).abs() <= f64::EPSILON);
        assert_eq!(step.order(), HERMITE_ORDER);

        let mut sampled = vec![0.0];
        plugin.sample(0.25, &mut sampled).expect("interior sample");
        assert!((sampled[0] - 0.25).abs() < 1.0e-12);
        assert!(plugin.sample(0.75, &mut sampled).is_err());
    }

    /// The retained handle survives across accepted steps: one `initialize`
    /// issues it and two later host calls evaluate through the *same* stored
    /// value. This is the property a per-call `&dyn` lend could not express and
    /// the reason a persistent implicit method needs the retained form.
    #[test]
    fn one_issued_handle_serves_every_later_host_call() {
        let controller = unit_rate_controller();
        let mut plugin = HermiteStepIntegrator::new(1, SamplerQuality::Native);
        let start = MeContinuousPoint::new(
            0.0,
            vec![0.0],
            MeContinuousStateDomain::verification_fixture(1),
        )
        .expect("checked point");
        {
            let _window = controller.activate();
            plugin
                .initialize(&start, controller.issue_handle())
                .expect("initialize");
        }
        let mut point = start;
        for _ in 0..3 {
            let request =
                MeAdvanceRequest::new(point.clone(), None, point.time() + 0.25, None, None)
                    .expect("checked request");
            let candidate = {
                let _window = controller.activate();
                plugin.advance(&request).expect("one accepted step")
            };
            let step = MeStepProposal::bind(request, candidate).expect("the candidate binds");
            point = step.accepted().clone();
        }
        assert!((point.states()[0] - 0.75).abs() < 1.0e-12);
        {
            let _window = controller.activate();
            plugin
                .truncate_reset(&point)
                .expect("the same retained handle serves truncate/reset");
        }
    }

    /// SPEC_0044 §6's ruling: retention is not reachability. A request issued
    /// from inside `sample` is a typed contract failure, not an evaluation at
    /// whatever coordinate the component happens to be standing on.
    #[test]
    fn a_component_request_from_inside_sample_is_refused() {
        let controller = unit_rate_controller();
        let mut plugin = HermiteStepIntegrator::new(1, SamplerQuality::ProbesInactiveCapability);
        let start = MeContinuousPoint::new(
            0.0,
            vec![0.0],
            MeContinuousStateDomain::verification_fixture(1),
        )
        .expect("checked point");
        {
            let _window = controller.activate();
            plugin
                .initialize(&start, controller.issue_handle())
                .expect("initialize");
        }
        let request = MeAdvanceRequest::new(start, None, 1.0, None, None).expect("checked request");
        {
            let _window = controller.activate();
            plugin.advance(&request).expect("one accepted step");
        }

        let mut sampled = vec![0.0];
        let failure = plugin
            .sample(0.5, &mut sampled)
            .expect_err("the capability is inactive during sample");
        // The plugin sees only the identity-free refusal...
        assert!(matches!(failure, MeIntegrationError::DerivativeRefused));
        // ...while the host-private controller holds the typed cause.
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::DerivativeCapabilityInactive { .. })
        ));
        assert!(!controller.is_active());
    }

    /// The same plugin, unchanged, is legal the moment the host opens a window
    /// for it: the refusal above is about *when*, not about the plugin.
    #[test]
    fn the_same_probe_is_legal_inside_an_open_window() {
        let controller = unit_rate_controller();
        let mut plugin = HermiteStepIntegrator::new(1, SamplerQuality::ProbesInactiveCapability);
        let start = MeContinuousPoint::new(
            0.0,
            vec![0.0],
            MeContinuousStateDomain::verification_fixture(1),
        )
        .expect("checked point");
        {
            let _window = controller.activate();
            plugin
                .initialize(&start, controller.issue_handle())
                .expect("initialize");
        }
        let request = MeAdvanceRequest::new(start, None, 1.0, None, None).expect("checked request");
        let _window = controller.activate();
        plugin.advance(&request).expect("one accepted step");
        let mut sampled = vec![0.0];
        plugin
            .sample(0.5, &mut sampled)
            .expect("an open window admits the same request");
    }

    #[test]
    fn a_cubic_trajectory_is_reproduced_exactly_by_the_declared_extension() {
        let error = max_sampler_error(SamplerQuality::Native, &CUBIC, 0.0, 0.2, 5)
            .expect("the exactness witness runs");
        assert!(
            error < 1.0e-13,
            "{} must be reproduced to roundoff, got {error}",
            CUBIC.name
        );
    }

    #[test]
    fn the_manufactured_solution_suite_confirms_the_declared_order() {
        for solution in [&SINE, &QUARTIC] {
            let order = order_over(SamplerQuality::Native, solution);
            assert!(
                order >= f64::from(HERMITE_ORDER) - 0.5,
                "{} attained order {order}, below the declared {HERMITE_ORDER}",
                solution.name
            );
        }
    }

    #[test]
    fn the_suite_rejects_a_deliberately_degraded_sampler() {
        for solution in [&SINE, &QUARTIC] {
            let order = order_over(SamplerQuality::DegradedLinear, solution);
            assert!(
                order < f64::from(HERMITE_ORDER) - 0.5,
                "{} must not appear to attain the declared order, got {order}",
                solution.name
            );
            assert!(
                order <= f64::from(DEGRADED_SAMPLER_ORDER) + 0.5,
                "a straight-line sampler attains order {order}"
            );
        }
    }

    #[test]
    fn endpoint_agreement_alone_cannot_distinguish_the_degraded_sampler() {
        // Both samplers agree with the accepted endpoints; only interior
        // convergence evidence separates them. This is why SPEC_0044 §6 splits
        // endpoint validation from order validation.
        let derivative: DerivativeClosure =
            Rc::new(|t, _y| vec![(std::f64::consts::TAU * t).cos()]);
        for quality in [SamplerQuality::Native, SamplerQuality::DegradedLinear] {
            let controller = MeDerivativeController::over_closure(1, Rc::clone(&derivative));
            let mut plugin = HermiteStepIntegrator::new(1, quality);
            let start = MeContinuousPoint::new(
                0.0,
                vec![0.0],
                MeContinuousStateDomain::verification_fixture(1),
            )
            .expect("checked point");
            {
                let _window = controller.activate();
                plugin
                    .initialize(&start, controller.issue_handle())
                    .expect("initialize");
            }
            let request =
                MeAdvanceRequest::new(start, None, 1.0, None, None).expect("checked request");
            let candidate = {
                let _window = controller.activate();
                plugin.advance(&request).expect("one accepted step")
            };
            let step = MeStepProposal::bind(request, candidate).expect("the candidate binds");
            let mut sampled = vec![0.0];
            for endpoint in [step.previous(), step.accepted()] {
                plugin
                    .sample(endpoint.time(), &mut sampled)
                    .expect("endpoint sample");
                assert!((sampled[0] - endpoint.states()[0]).abs() < 1.0e-12);
            }
        }
    }

    #[test]
    fn the_time_only_plugin_satisfies_the_same_contract_vacuously() {
        let mut plugin = TimeOnlyIntegrator::new();
        let start = MeContinuousPoint::new(
            0.0,
            Vec::new(),
            MeContinuousStateDomain::verification_fixture(0),
        )
        .expect("empty point");
        plugin
            .initialize(&start, detached_handle())
            .expect("initialize");
        let request = MeAdvanceRequest::new(start, None, 1.0, None, None).expect("request");
        let candidate = plugin.advance(&request).expect("one accepted step");
        let step = MeStepProposal::bind(request, candidate).expect("the candidate binds");
        assert!(step.order() > 0);
        // Zero state: every coordinate inside the interval is exact, and every
        // coordinate outside it is refused, exactly as for the unrelated
        // plugin above.
        assert!(plugin.sample(0.5, &mut []).is_ok());
        assert!(plugin.sample(1.5, &mut []).is_err());
    }

    #[test]
    fn a_plugin_failure_is_typed_rather_than_rendered_prose() {
        let plugin = HermiteStepIntegrator::new(1, SamplerQuality::Native);
        let mut sampled = vec![0.0];
        let failure = plugin
            .sample(0.5, &mut sampled)
            .expect_err("no interval has been accepted yet");
        assert!(matches!(
            failure,
            MeIntegrationError::Numerical {
                category: MeNumericalFailure::Interpolation,
                ..
            }
        ));
    }
}
