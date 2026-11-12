//! The fixed-step plugin driven through the solver-neutral public ODE driver,
//! which issues the same retained handle, activation window and checked
//! request the common session does.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use rumoca_solver::fmi_me::ode_driver::{ContinuousOde, OdeDriveError, OdeRun, integrate_ode};
use rumoca_solver::{BackendOptionsError, NumericalBackend};

use super::*;

const ORDERS: [u32; 3] = [1, 2, 4];

/// `x' = v`, `v' = -x` from `(1, 0)`: the solution is `(cos t, -sin t)`.
#[derive(Default)]
struct Oscillator {
    evaluations: Cell<usize>,
    /// The evaluation, counted from one, the system refuses.
    refuse_at: Option<usize>,
}

impl ContinuousOde for Oscillator {
    fn width(&self) -> usize {
        2
    }

    fn derivatives_into(&self, _time: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        let count = self.evaluations.get() + 1;
        self.evaluations.set(count);
        if self.refuse_at == Some(count) {
            return Err("the system refuses this evaluation".to_string());
        }
        out[0] = state[1];
        out[1] = -state[0];
        Ok(())
    }
}

fn exact(time: f64) -> [f64; 2] {
    [time.cos(), -time.sin()]
}

fn error_at(time: f64, state: &[f64]) -> f64 {
    let exact = exact(time);
    (state[0] - exact[0]).hypot(state[1] - exact[1])
}

fn run(t_end: f64) -> OdeRun {
    OdeRun {
        relative_tolerance: 1.0e-6,
        absolute_tolerance: 1.0e-9,
        nominals: vec![1.0; 2],
        initial_step: None,
        t_start: 0.0,
        t_end,
        initial_state: vec![1.0, 0.0],
        breakpoints: Vec::new(),
        extension_order_limit: None,
    }
}

fn plugin(setup: MeNumericalSetup, step: f64, order: u32) -> Box<dyn MeIntegratorBackend> {
    fixed_step_integrator(
        setup,
        FixedStepConfig {
            step,
            order,
            scheme: FixedStepScheme::Explicit,
        },
    )
    .expect("a valid configuration")
}

/// What the driver observed of one run: every accepted endpoint, the
/// evaluations spent by then, each step's declared extension order and the
/// continuous extension at each interval's midpoint.
#[derive(Debug, Default, PartialEq)]
struct Trace {
    end_times: Vec<u64>,
    end_states: Vec<Vec<u64>>,
    evaluations: Vec<usize>,
    orders: Vec<u32>,
    midpoints: Vec<Vec<u64>>,
}

impl Trace {
    fn final_state(&self) -> Vec<f64> {
        let last = self.end_states.last().expect("at least one step");
        last.iter().copied().map(f64::from_bits).collect()
    }
}

fn integrate_with(
    system: &Rc<Oscillator>,
    t_end: f64,
    build: &dyn Fn(MeNumericalSetup) -> Box<dyn MeIntegratorBackend>,
) -> Result<Trace, OdeDriveError> {
    let mut trace = Trace::default();
    let source: Rc<dyn ContinuousOde> = Rc::clone(system) as Rc<dyn ContinuousOde>;
    integrate_ode(source, &run(t_end), build, |interval| {
        trace.end_times.push(interval.end_time().to_bits());
        trace
            .end_states
            .push(interval.end_state().iter().map(|v| v.to_bits()).collect());
        trace.evaluations.push(system.evaluations.get());
        trace.orders.push(interval.extension_order());
        let mut midpoint = [0.0; 2];
        interval
            .sample(
                0.5 * (interval.start_time() + interval.end_time()),
                &mut midpoint,
            )
            .map_err(|error| error.to_string())?;
        trace.midpoints.push(bits(&midpoint));
        Ok(())
    })?;
    Ok(trace)
}

fn integrate(t_end: f64, step: f64, order: u32) -> (Trace, usize) {
    let system = Rc::new(Oscillator::default());
    let trace = integrate_with(&system, t_end, &|setup| plugin(setup, step, order))
        .expect("the oscillator integrates");
    (trace, system.evaluations.get())
}

#[test]
fn each_order_spends_exactly_its_stage_count_on_every_step() {
    // Eight full steps, then three full steps and a clipped one.
    for (step, steps) in [(0.125, 8), (0.3, 4)] {
        for order in ORDERS {
            let stages = order as usize;
            let (trace, total) = integrate(1.0, step, order);
            assert_eq!(trace.end_times.len(), steps, "order {order} step {step}");
            assert_eq!(total, steps * stages, "order {order} step {step}");
            let per_step: Vec<usize> = (1..=steps).map(|index| index * stages).collect();
            assert_eq!(trace.evaluations, per_step, "order {order} step {step}");
        }
    }
}

/// The observed order between successive halvings of the step, at `t = 1`.
fn observed_orders(order: u32) -> Vec<f64> {
    let errors: Vec<f64> = [16.0, 32.0, 64.0, 128.0]
        .iter()
        .map(|steps: &f64| {
            let (trace, _) = integrate(1.0, 1.0 / steps, order);
            error_at(1.0, &trace.final_state())
        })
        .collect();
    errors
        .windows(2)
        .map(|pair| (pair[0] / pair[1]).log2())
        .collect()
}

#[test]
fn step_halving_converges_at_the_configured_order() {
    for order in ORDERS {
        let expected = f64::from(order);
        for observed in observed_orders(order) {
            assert!(
                (observed - expected).abs() < 0.25,
                "order {order}: observed {observed:.3}"
            );
        }
    }
}

/// A step taken in place of the probed plugin's own, with the derivative
/// capability withheld from that plugin.
pub(super) type StepHook = Box<
    dyn Fn(&MeDerivativeHandle, &MeAdvanceRequest) -> Result<MeStepCandidate, MeIntegrationError>,
>;

/// The plugin under test and what [`Shared`] records of it: each step as the
/// plugin returned it, before the host bound or normalized anything.
struct Probe {
    inner: Box<dyn MeIntegratorBackend>,
    raw_steps: Rc<RefCell<Vec<(f64, f64)>>>,
    /// Truncate the plugin at the current point before every step.
    reset_before_each_step: bool,
    /// Keep the derivative capability the host issues at `initialize` from
    /// the plugin, as a composing plugin that forwards steps but not the
    /// capability would.
    withholds_capability: bool,
    /// The capability withheld from the plugin.
    withheld: Option<MeDerivativeHandle>,
    /// Every step, when set, instead of the plugin's own.
    step_hook: Option<StepHook>,
}

impl Probe {
    fn assert_no_retained_interval(&self, time: f64) {
        let mut sampled = [0.0; 2];
        let error = self
            .inner
            .sample(time, &mut sampled)
            .expect_err("a restarted plugin retains no interval");
        assert!(matches!(
            error,
            MeIntegrationError::Numerical {
                method: METHOD,
                category: MeNumericalFailure::Interpolation,
                ..
            }
        ));
    }
}

/// The plugin as the host holds it: forwards every call to the probed plugin.
/// One probe can be shared by several driven runs, so a later run
/// re-initializes a plugin that still holds the previous run's history. A
/// truncation, the host's or the one a reset-mode step emulates, enters
/// through [`MeIntegratorBackend::truncate_reset`] here and must leave no
/// interval behind.
pub(super) struct Shared(Rc<RefCell<Probe>>);

impl Shared {
    /// The wrapper over `inner` whose every step is `hook`, run with the
    /// capability withheld from `inner`.
    pub(super) fn stepping_with(inner: Box<dyn MeIntegratorBackend>, hook: StepHook) -> Self {
        Self(Rc::new(RefCell::new(Probe {
            inner,
            raw_steps: Rc::default(),
            reset_before_each_step: false,
            withholds_capability: true,
            withheld: None,
            step_hook: Some(hook),
        })))
    }
}

impl MeIntegratorBackend for Shared {
    fn continuous_extension_order(&self) -> Option<u32> {
        self.0.borrow().inner.continuous_extension_order()
    }

    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        let mut probe = self.0.borrow_mut();
        if probe.withholds_capability {
            probe.withheld = Some(derivatives);
        } else {
            probe.inner.initialize(point, derivatives)?;
        }
        probe.assert_no_retained_interval(point.time());
        Ok(())
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        if self.0.borrow().reset_before_each_step {
            self.truncate_reset(request.current())?;
        }
        let probe = self.0.borrow();
        if let Some(hook) = &probe.step_hook {
            let derivatives = probe
                .withheld
                .as_ref()
                .expect("a hooked step runs with the withheld capability");
            return hook(derivatives, request);
        }
        drop(probe);
        let mut probe = self.0.borrow_mut();
        let candidate = probe.inner.advance(request)?;
        probe
            .raw_steps
            .borrow_mut()
            .push((request.current().time(), candidate.accepted_time()));
        Ok(candidate)
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        self.0.borrow().inner.sample(time, states)
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        let mut probe = self.0.borrow_mut();
        probe.inner.truncate_reset(point)?;
        probe.assert_no_retained_interval(point.time());
        Ok(())
    }
}

fn probe(step: f64, order: u32, reset_before_each_step: bool) -> Rc<RefCell<Probe>> {
    let setup = MeNumericalSetup::new(1.0e-6, 1.0e-9, vec![1.0; 2], 2, None)
        .expect("fixture setup is checked");
    Rc::new(RefCell::new(Probe {
        inner: plugin(setup, step, order),
        raw_steps: Rc::default(),
        reset_before_each_step,
        withholds_capability: false,
        withheld: None,
        step_hook: None,
    }))
}

fn integrate_shared(shared: &Rc<RefCell<Probe>>, t_end: f64) -> (Trace, usize) {
    let system = Rc::new(Oscillator::default());
    let trace = integrate_with(&system, t_end, &|_| Box::new(Shared(Rc::clone(shared))))
        .expect("the oscillator integrates");
    (trace, system.evaluations.get())
}

#[test]
fn a_clipped_last_step_lands_exactly_on_the_requested_time() {
    for order in ORDERS {
        let shared = probe(0.3, order, false);
        integrate_shared(&shared, 1.0);
        let raw = shared.borrow().raw_steps.borrow().clone();
        assert_eq!(raw.len(), 4);
        for &(start, end) in &raw[..3] {
            assert_eq!(end.to_bits(), (start + 0.3).to_bits());
        }
        let (start, end) = raw[3];
        assert_eq!(end.to_bits(), 1.0_f64.to_bits());
        assert!(end - start < 0.3);
    }
}

#[test]
fn the_step_is_clipped_to_the_reachable_bound_or_refused() {
    assert_eq!(bounded_step(1.0, 2.0, None, 0.25), Some((0.25, 1.25)));
    assert_eq!(
        bounded_step(1.0, 1.5, Some(0.125), 1.0),
        Some((0.125, 1.125))
    );
    // A step clipped to the remaining interval ends on the bound itself.
    assert_eq!(bounded_step(1.0, 1.05, None, 1.0), Some((1.05 - 1.0, 1.05)));
    assert_eq!(bounded_step(0.75, 1.0, None, 0.25), Some((0.25, 1.0)));
    // At t = 1e6 a 2.5e-11 s step rounds back to t.
    assert_eq!(bounded_step(1.0e6, 2.0e6, None, 2.5e-11), None);
    assert_eq!(bounded_step(1.0, 2.0, None, 0.0), None);
    assert_eq!(bounded_step(1.0, 2.0, None, f64::NAN), None);
    assert_eq!(bounded_step(1.0, 1.0, None, 0.25), None);
}

#[test]
fn a_truncated_plugin_repeats_the_same_steps_at_the_same_cost() {
    for order in ORDERS {
        let plain = integrate(1.0, 0.125, order);
        let reset = integrate_shared(&probe(0.125, order, true), 1.0);
        assert_eq!(reset, plain, "order {order}");
    }
}

#[test]
fn a_reinitialized_plugin_restarts_without_the_previous_history() {
    for order in ORDERS {
        let fresh = integrate(0.5, 0.125, order);
        let shared = probe(0.125, order, false);
        // The first run leaves an accepted interval ending at t = 0.75 behind.
        integrate_shared(&shared, 0.75);
        let again = integrate_shared(&shared, 0.5);
        assert_eq!(again, fresh, "order {order}");
    }
}

/// A plugin asked to step without the derivative capability the host issues
/// at `initialize` refuses with its typed construction failure before it
/// evaluates anything or accepts a step.
#[test]
fn a_step_without_the_issued_capability_is_the_typed_construction_refusal() {
    for order in ORDERS {
        let shared = probe(0.125, order, false);
        shared.borrow_mut().withholds_capability = true;
        let system = Rc::new(Oscillator::default());
        let outcome = integrate_with(&system, 1.0, &|_| Box::new(Shared(Rc::clone(&shared))));
        assert!(
            matches!(
                outcome,
                Err(OdeDriveError::Integration(MeIntegrationError::Numerical {
                    method: METHOD,
                    category: MeNumericalFailure::Construction,
                    ..
                }))
            ),
            "order {order}: {outcome:?}"
        );
        assert_eq!(system.evaluations.get(), 0, "order {order}");
        assert!(shared.borrow().raw_steps.borrow().is_empty());
    }
}

/// The interior error of one step of length `step` from exact data.
fn one_step_interior_error(order: u32, step: f64) -> f64 {
    let system: Rc<dyn ContinuousOde> = Rc::new(Oscillator::default());
    let mut worst = 0.0_f64;
    let extension_order = Method::from_order(order)
        .expect("a supported order")
        .continuous_extension_order();
    integrate_ode(
        system,
        &run(step),
        &|setup| plugin(setup, step, order),
        |interval| {
            assert_eq!(interval.extension_order(), extension_order);
            let mut sampled = [0.0; 2];
            for (time, expected) in [
                (interval.start_time(), interval.start_state()),
                (interval.end_time(), interval.end_state()),
            ] {
                interval
                    .sample(time, &mut sampled)
                    .map_err(|e| e.to_string())?;
                assert_eq!(sampled.map(f64::to_bits).as_slice(), bits(expected));
            }
            for theta in [0.3, 0.7] {
                let time = theta * step;
                interval
                    .sample(time, &mut sampled)
                    .map_err(|e| e.to_string())?;
                worst = worst.max(error_at(time, &sampled));
            }
            Ok(())
        },
    )
    .expect("one step integrates");
    worst
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

/// A continuous extension of local order `q` leaves an interior error of
/// `O(h^(q+1))` after one step from exact data. A chord under the two- or
/// four-stage method would show 2 here instead of 3 or 4.
#[test]
fn the_continuous_extension_meets_both_ends_and_has_its_declared_order() {
    for (order, declared) in [(1, 1), (2, 2), (4, 3)] {
        let errors = [0.125, 0.0625, 0.03125].map(|step| one_step_interior_error(order, step));
        for pair in errors.windows(2) {
            let observed = (pair[0] / pair[1]).log2();
            let expected = f64::from(declared) + 1.0;
            assert!(
                (observed - expected).abs() < 0.25,
                "order {order}: observed local order {observed:.3}, declared {declared}"
            );
        }
    }
}

#[test]
fn the_declared_extension_order_is_known_before_the_first_step() {
    for (order, declared) in [(1, 1), (2, 2), (4, 3)] {
        let shared = probe(0.125, order, false);
        assert_eq!(
            shared.borrow().inner.continuous_extension_order(),
            Some(declared)
        );
        shared.borrow().assert_no_retained_interval(0.0);
    }
}

#[test]
fn a_refused_evaluation_ends_the_step_without_a_retry() {
    for (order, refuse_at) in [(1, 1), (2, 2), (4, 3), (4, 7)] {
        let system = Rc::new(Oscillator {
            refuse_at: Some(refuse_at),
            ..Oscillator::default()
        });
        let outcome = integrate_with(&system, 1.0, &|setup| plugin(setup, 0.125, order));
        assert!(
            matches!(outcome, Err(OdeDriveError::Integration(_))),
            "order {order}: {outcome:?}"
        );
        // Nothing is evaluated after the refusal: no later stage, no retry.
        assert_eq!(system.evaluations.get(), refuse_at, "order {order}");
    }
}

#[test]
fn an_unvalidated_configuration_is_refused_by_the_factory() {
    let setup = || {
        MeNumericalSetup::new(1.0e-6, 1.0e-9, vec![1.0; 2], 2, None)
            .expect("fixture setup is checked")
    };
    for (config, field) in [
        (
            FixedStepConfig {
                step: 1.0e-3,
                order: 3,
                scheme: FixedStepScheme::Explicit,
            },
            "order",
        ),
        (
            FixedStepConfig {
                step: 0.0,
                order: 4,
                scheme: FixedStepScheme::Explicit,
            },
            "step",
        ),
    ] {
        let error = fixed_step_integrator(setup(), config)
            .err()
            .expect("an invalid configuration builds no plugin");
        assert_eq!(
            error,
            BackendOptionsError::InvalidValue {
                backend: NumericalBackend::FixedStep,
                field,
            }
        );
    }
}

#[test]
fn continuous_extension_allocation_is_typed_and_preserves_existing_values() {
    let mut fresh = HermiteInterval::default();
    assert!(matches!(
        fresh.reserve(usize::MAX, true),
        Err(MeIntegrationError::Allocation {
            entries: usize::MAX,
            ..
        })
    ));
    let mut retained = HermiteInterval {
        start_time: 1.0,
        end_time: 2.0,
        step: 1.0,
        start: vec![2.0],
        end: vec![3.0],
        start_slope: vec![1.0],
        end_slope: vec![1.0],
    };
    assert!(matches!(
        retained.reserve(usize::MAX, true),
        Err(MeIntegrationError::Allocation {
            entries: usize::MAX,
            ..
        })
    ));
    assert_eq!(retained.start_time, 1.0);
    assert_eq!(retained.end_time, 2.0);
    assert_eq!(retained.step, 1.0);
    assert_eq!(retained.start, [2.0]);
    assert_eq!(retained.end, [3.0]);
    assert_eq!(retained.start_slope, [1.0]);
    assert_eq!(retained.end_slope, [1.0]);
    retained.reserve(8, true).unwrap();
    for capacity in [
        retained.start.capacity(),
        retained.end.capacity(),
        retained.start_slope.capacity(),
        retained.end_slope.capacity(),
    ] {
        assert!(capacity >= 8);
    }
}
