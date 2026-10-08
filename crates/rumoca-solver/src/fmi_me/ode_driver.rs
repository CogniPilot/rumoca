//! A solver-neutral initial-value driver over the numerical plugin contract
//! (SPEC_0044 §6, ME-INT-001).
//!
//! The session drives a plugin against one FMI component. A derived system
//! that is not a component, such as the forward sensitivity equations or the
//! adjoint equations assembled from Solve directional-derivative rows, still
//! needs the same plugins, so this driver issues the same retained handle,
//! opens the same activation window around exactly `initialize` and one
//! `advance`, binds every candidate to the request it answered, and joins a
//! latched derivative failure ahead of whatever the plugin returned. It adds
//! no event, root, or trace policy: the derived systems it serves are
//! continuous by construction (the callers prove that before they build one).
//!
//! The plugin is unchanged: the same Dormand-Prince plugin that advances a
//! simulated model advances the sensitivity system.

use std::rc::Rc;

use super::MeError;
use super::integrator::{
    MeAdvanceRequest, MeContinuousPoint, MeDerivativeComponent, MeDerivativeController,
    MeIntegrationError, MeIntegratorBackend, MeNumericalSetup, MeStepProposal, sample_complete,
};

/// A continuous system `y' = f(t, y)` of fixed width.
pub trait ContinuousOde {
    /// Number of continuous states.
    fn width(&self) -> usize;

    /// Write `f(time, state)` into `out`; a failure is a rendered message.
    fn derivatives_into(&self, time: f64, state: &[f64], out: &mut [f64]) -> Result<(), String>;
}

/// The derivative source a plugin evaluates through its retained handle.
struct OdeComponent {
    system: Rc<dyn ContinuousOde>,
}

impl MeDerivativeComponent for OdeComponent {
    fn state_count(&self) -> usize {
        self.system.width()
    }

    fn derivatives_into(
        &self,
        time: f64,
        states: &[f64],
        _event_boundary: Option<f64>,
        out: &mut [f64],
    ) -> Result<(), MeError> {
        self.system
            .derivatives_into(time, states, out)
            .map_err(|message| MeError::Evaluation { message })?;
        if let Some(index) = out.iter().position(|value| !value.is_finite()) {
            return Err(MeError::Evaluation {
                message: format!("derivative {index} is not finite at t={time}"),
            });
        }
        Ok(())
    }

    fn directional_derivative_into(
        &self,
        _time: f64,
        _states: &[f64],
        _event_boundary: Option<f64>,
        _seed: &[f64],
        _out: &mut [f64],
    ) -> Result<(), MeError> {
        Err(MeError::DirectionalDerivativeUnavailable {
            reason: "a derived sensitivity system supplies no second-order linearization"
                .to_owned(),
        })
    }

    fn discards_failed_trials(&self) -> bool {
        false
    }

    fn state_jacobian_columns(&self) -> Option<&[Vec<usize>]> {
        None
    }
}

/// Failure of one driven integration.
#[derive(Debug, thiserror::Error)]
pub enum OdeDriveError {
    /// The numerical plugin or the derivative source failed.
    #[error("{0}")]
    Integration(#[from] MeIntegrationError),
    /// The caller's observer rejected an accepted interval.
    #[error("{0}")]
    Observer(String),
    /// The plugin declares no continuous-extension order within the limit the
    /// run requires, so the run was refused before its first step.
    #[error(
        "the plugin's declared continuous-extension order {declared:?} is not within the \
         required limit {limit}"
    )]
    ExtensionOrder { declared: Option<u32>, limit: u32 },
}

/// Numerical configuration and span of one driven integration.
#[derive(Debug, Clone)]
pub struct OdeRun {
    pub relative_tolerance: f64,
    pub absolute_tolerance: f64,
    /// One positive nominal per state, which scales the plugin's tolerances.
    pub nominals: Vec<f64>,
    pub initial_step: Option<f64>,
    pub t_start: f64,
    pub t_end: f64,
    pub initial_state: Vec<f64>,
    /// Ascending times strictly inside `(t_start, t_end)` that every accepted
    /// step must not cross: a step ends at the next one. A caller whose
    /// integrand or interpolant changes form at a time (a data knot) lists it,
    /// so no feature narrower than a step is skipped.
    pub breakpoints: Vec<f64>,
    /// The highest continuous-extension order the caller can use. A plugin
    /// whose declared order is above it, or that declares none, is refused
    /// before the first step ([`OdeDriveError::ExtensionOrder`]).
    pub extension_order_limit: Option<u32>,
}

/// One accepted interval and the plugin's native continuous extension over it.
pub struct OdeInterval<'a> {
    t0: f64,
    t1: f64,
    state0: &'a [f64],
    state1: &'a [f64],
    extension_order: u32,
    plugin: &'a dyn MeIntegratorBackend,
}

impl OdeInterval<'_> {
    #[must_use]
    pub fn start_time(&self) -> f64 {
        self.t0
    }

    #[must_use]
    pub fn end_time(&self) -> f64 {
        self.t1
    }

    #[must_use]
    pub fn start_state(&self) -> &[f64] {
        self.state0
    }

    #[must_use]
    pub fn end_state(&self) -> &[f64] {
        self.state1
    }

    /// The accuracy order the plugin declares for its continuous extension over
    /// this interval.
    #[must_use]
    pub fn extension_order(&self) -> u32 {
        self.extension_order
    }

    /// Evaluate the plugin's continuous extension at `time` inside the
    /// interval; the plugin writes every entry or the call fails.
    pub fn sample(&self, time: f64, out: &mut [f64]) -> Result<(), OdeDriveError> {
        sample_complete(self.plugin, time, out).map_err(OdeDriveError::from)
    }
}

/// Integrate `system` over `run`, handing each accepted interval to `observer`.
///
/// `build` constructs the plugin from the checked numerical setup, exactly as
/// the common host does for a simulated component.
pub fn integrate_ode(
    system: Rc<dyn ContinuousOde>,
    run: &OdeRun,
    build: &dyn Fn(MeNumericalSetup) -> Box<dyn MeIntegratorBackend>,
    mut observer: impl FnMut(&OdeInterval<'_>) -> Result<(), String>,
) -> Result<(), OdeDriveError> {
    let width = system.width();
    let setup = MeNumericalSetup::new(
        run.relative_tolerance,
        run.absolute_tolerance,
        run.nominals.clone(),
        width,
        run.initial_step,
    )?;
    let controller = MeDerivativeController::over_component(Box::new(OdeComponent { system }));
    let mut plugin = build(setup);
    let declared = plugin.continuous_extension_order();
    if let Some(limit) = run.extension_order_limit
        && declared.is_none_or(|order| order > limit)
    {
        return Err(OdeDriveError::ExtensionOrder { declared, limit });
    }
    let mut point = MeContinuousPoint::new(run.t_start, run.initial_state.clone(), width)?;

    let started = {
        let window = controller.activate_until(None);
        let outcome = plugin.initialize(&point, controller.issue_handle());
        drop(window);
        outcome
    };
    settle_call(&controller, started)?;

    while point.time() < run.t_end {
        let stop = next_stop(run, point.time());
        let request = MeAdvanceRequest::new(point.clone(), Some(stop), run.t_end, None, None)?;
        let advanced = {
            let window = controller.activate_until(None);
            let outcome = plugin.advance(&request);
            drop(window);
            outcome
        };
        let candidate = settle_call(&controller, advanced)?;
        let proposal = MeStepProposal::bind(request, candidate, width)?;
        if declared.is_some_and(|order| proposal.order() > order) {
            return Err(MeIntegrationError::contract(format!(
                "a step declared continuous-extension order {} above the plugin's declared \
                 bound {declared:?}",
                proposal.order()
            ))
            .into());
        }
        let accepted = proposal.accepted().clone();
        observer(&OdeInterval {
            t0: proposal.previous().time(),
            t1: accepted.time(),
            state0: proposal.previous().states(),
            state1: accepted.states(),
            extension_order: proposal.order(),
            plugin: plugin.as_ref(),
        })
        .map_err(OdeDriveError::Observer)?;
        point = accepted;
    }
    Ok(())
}

/// The next time a step from `now` must not cross: the first breakpoint beyond
/// round-off, else the end of the run.
fn next_stop(run: &OdeRun, now: f64) -> f64 {
    let roundoff = super::accepted_step_roundoff(now, 0.0);
    run.breakpoints
        .iter()
        .copied()
        .find(|stop| *stop - now > roundoff)
        .map_or(run.t_end, |stop| stop.min(run.t_end))
}

/// Join a latched derivative failure ahead of the plugin's own result, so a
/// generic library error can never hide the typed cause that produced it.
fn settle_call<T>(
    controller: &MeDerivativeController,
    outcome: Result<T, MeIntegrationError>,
) -> Result<T, MeIntegrationError> {
    if let Some(latched) = controller.take_error() {
        return Err(latched);
    }
    if let Some(cause) = controller.settle_discard(outcome.is_ok()) {
        return Err(cause);
    }
    outcome
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmi_me::integrator::conformance::{HermiteStepIntegrator, SamplerQuality};

    /// `y' = -y`.
    struct Decay;

    impl ContinuousOde for Decay {
        fn width(&self) -> usize {
            1
        }

        fn derivatives_into(
            &self,
            _time: f64,
            state: &[f64],
            out: &mut [f64],
        ) -> Result<(), String> {
            out[0] = -state[0];
            Ok(())
        }
    }

    /// A system whose derivative refuses to evaluate.
    struct Broken(f64);

    impl ContinuousOde for Broken {
        fn width(&self) -> usize {
            1
        }

        fn derivatives_into(
            &self,
            _time: f64,
            _state: &[f64],
            out: &mut [f64],
        ) -> Result<(), String> {
            if self.0.is_nan() {
                out[0] = self.0;
                Ok(())
            } else {
                Err("the system cannot be evaluated".to_string())
            }
        }
    }

    fn run(t_end: f64) -> OdeRun {
        OdeRun {
            relative_tolerance: 1.0e-6,
            absolute_tolerance: 1.0e-9,
            nominals: vec![1.0],
            initial_step: None,
            t_start: 0.0,
            t_end,
            initial_state: vec![1.0],
            breakpoints: Vec::new(),
            extension_order_limit: None,
        }
    }

    fn hermite(setup: MeNumericalSetup) -> Box<dyn MeIntegratorBackend> {
        Box::new(HermiteStepIntegrator::new(
            setup.state_nominals().len(),
            SamplerQuality::Native,
        ))
    }

    #[test]
    fn a_plugin_advances_a_derived_system_through_the_retained_handle() {
        let mut intervals = Vec::new();
        integrate_ode(Rc::new(Decay), &run(1.0), &hermite, |interval| {
            let mut midpoint = [0.0];
            interval
                .sample(
                    0.5 * (interval.start_time() + interval.end_time()),
                    &mut midpoint,
                )
                .map_err(|error| error.to_string())?;
            intervals.push((
                interval.start_time(),
                interval.end_time(),
                interval.start_state()[0],
                interval.end_state()[0],
                midpoint[0],
            ));
            Ok(())
        })
        .expect("decay integrates");
        let (start, end, y0, y1, middle) = intervals[0];
        assert_eq!((start, end, y0), (0.0, 1.0, 1.0));
        // One classical RK4 step of `y' = -y` over a unit interval.
        assert!((y1 - 0.375).abs() < 1.0e-12, "y1 = {y1}");
        assert!(middle > y1 && middle < y0, "middle = {middle}");
    }

    #[test]
    fn a_plugin_above_the_required_extension_order_is_refused_before_any_step() {
        let mut steps = 0;
        let mut limited = run(1.0);
        limited.extension_order_limit = Some(3);
        let outcome = integrate_ode(Rc::new(Decay), &limited, &hermite, |_| {
            steps += 1;
            Ok(())
        });
        assert!(matches!(
            outcome,
            Err(OdeDriveError::ExtensionOrder {
                declared: Some(4),
                limit: 3
            })
        ));
        assert_eq!(steps, 0);
        limited.extension_order_limit = Some(4);
        assert!(integrate_ode(Rc::new(Decay), &limited, &hermite, |_| Ok(())).is_ok());
    }

    #[test]
    fn a_plugin_declaring_no_extension_order_is_refused_when_one_is_required() {
        let build = |setup: MeNumericalSetup| -> Box<dyn MeIntegratorBackend> {
            Box::new(
                HermiteStepIntegrator::new(setup.state_nominals().len(), SamplerQuality::Native)
                    .with_declared_order(None),
            )
        };
        let mut limited = run(1.0);
        limited.extension_order_limit = Some(4);
        assert!(matches!(
            integrate_ode(Rc::new(Decay), &limited, &build, |_| Ok(())),
            Err(OdeDriveError::ExtensionOrder {
                declared: None,
                limit: 4
            })
        ));
        limited.extension_order_limit = None;
        assert!(integrate_ode(Rc::new(Decay), &limited, &build, |_| Ok(())).is_ok());
    }

    #[test]
    fn every_breakpoint_ends_a_step_and_the_plugin_declares_its_extension_order() {
        let mut ends = Vec::new();
        let mut orders = Vec::new();
        let mut with_stops = run(1.0);
        with_stops.breakpoints = vec![0.25, 0.5, 0.75];
        integrate_ode(Rc::new(Decay), &with_stops, &hermite, |interval| {
            ends.push(interval.end_time());
            orders.push(interval.extension_order());
            Ok(())
        })
        .expect("decay integrates");
        assert_eq!(ends, [0.25, 0.5, 0.75, 1.0]);
        assert!(orders.iter().all(|order| *order == 4), "{orders:?}");
    }

    #[test]
    fn a_breakpoint_beyond_the_end_or_at_the_start_does_not_stop_a_step() {
        let mut odd = run(1.0);
        odd.breakpoints = vec![0.0, 2.0];
        assert_eq!(next_stop(&odd, 0.0), 1.0);
        assert_eq!(next_stop(&odd, 0.5), 1.0);
    }

    #[test]
    fn the_observer_can_stop_the_run_with_its_own_message() {
        let error = integrate_ode(Rc::new(Decay), &run(1.0), &hermite, |_| {
            Err("observer stop".to_string())
        })
        .expect_err("the observer refused the interval");
        assert!(
            matches!(error, OdeDriveError::Observer(ref message) if message == "observer stop")
        );
    }

    #[test]
    fn a_failing_derivative_surfaces_its_typed_cause_ahead_of_the_plugin_result() {
        let error = integrate_ode(Rc::new(Broken(0.0)), &run(1.0), &hermite, |_| Ok(()))
            .expect_err("the derivative fails");
        assert!(error.to_string().contains("cannot be evaluated"), "{error}");
        let error = integrate_ode(Rc::new(Broken(f64::NAN)), &run(1.0), &hermite, |_| Ok(()))
            .expect_err("a non-finite derivative is refused");
        assert!(error.to_string().contains("not finite"), "{error}");
    }

    #[test]
    fn an_invalid_numerical_setup_is_refused_before_any_step() {
        let mut bad = run(1.0);
        bad.nominals = vec![0.0];
        let error = integrate_ode(Rc::new(Decay), &bad, &hermite, |_| Ok(()))
            .expect_err("a zero nominal is not a scale");
        assert!(matches!(error, OdeDriveError::Integration(_)), "{error}");
    }

    #[test]
    fn the_derived_system_publishes_no_linearization_for_an_implicit_plugin() {
        let component = OdeComponent {
            system: Rc::new(Decay),
        };
        assert!(component.state_jacobian_columns().is_none());
        assert!(!component.discards_failed_trials());
        let mut out = [0.0];
        let error = component
            .directional_derivative_into(0.0, &[1.0], None, &[1.0], &mut out)
            .expect_err("no second-order linearization exists");
        assert!(matches!(
            error,
            MeError::DirectionalDerivativeUnavailable { .. }
        ));
        component
            .derivatives_into(0.0, &[2.0], None, &mut out)
            .expect("evaluates");
        assert_eq!(out, [-2.0]);
        assert_eq!(component.state_count(), 1);
    }
}
