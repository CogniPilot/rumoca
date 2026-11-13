//! The semi-implicit scheme driven through the public ODE driver, over a
//! system that declares its kinematic states the way a component serves the
//! Solve IR partition.

use std::cell::Cell;
use std::rc::Rc;

use rumoca_solver::fmi_me::ode_driver::{ContinuousOde, OdeDriveError, OdeRun, integrate_ode};
use rumoca_solver::{BackendOptionsError, NumericalBackend};

use super::tests::{Shared, StepHook};
use super::*;

/// `x' = v`, `v' = -x` from `(1, 0)` with `kinematic` declared. The kinematic
/// derivative of `x` is `v` at the state it is given.
struct Oscillator {
    kinematic: Vec<usize>,
    full: Cell<usize>,
    selected: Cell<usize>,
}

impl Oscillator {
    fn new(kinematic: Vec<usize>) -> Rc<Self> {
        Rc::new(Self {
            kinematic,
            full: Cell::new(0),
            selected: Cell::new(0),
        })
    }
}

impl ContinuousOde for Oscillator {
    fn width(&self) -> usize {
        2
    }

    fn derivatives_into(&self, _time: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        self.full.set(self.full.get() + 1);
        out[0] = state[1];
        out[1] = -state[0];
        Ok(())
    }

    fn kinematic_states(&self) -> &[usize] {
        &self.kinematic
    }

    fn kinematic_derivatives_into(
        &self,
        _time: f64,
        state: &[f64],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.selected.set(self.selected.get() + 1);
        for &index in &self.kinematic {
            out[index] = if index == 0 { state[1] } else { -state[0] };
        }
        Ok(())
    }
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

fn config(step: f64, scheme: FixedStepScheme) -> FixedStepConfig {
    FixedStepConfig {
        step,
        order: 1,
        scheme,
    }
}

/// Every accepted endpoint, with the full and selected evaluations spent by
/// then.
fn endpoints(
    system: &Rc<Oscillator>,
    t_end: f64,
    config: FixedStepConfig,
) -> Result<Vec<(Vec<f64>, usize, usize)>, OdeDriveError> {
    let mut points = Vec::new();
    let source: Rc<dyn ContinuousOde> = Rc::clone(system) as Rc<dyn ContinuousOde>;
    integrate_ode(
        source,
        &run(t_end),
        &|setup| fixed_step_integrator(setup, config).expect("a valid configuration"),
        |interval| {
            assert_eq!(interval.extension_order(), 1);
            points.push((
                interval.end_state().to_vec(),
                system.full.get(),
                system.selected.get(),
            ));
            Ok(())
        },
    )?;
    Ok(points)
}

fn energy(state: &[f64]) -> f64 {
    0.5 * (state[0] * state[0] + state[1] * state[1])
}

/// The largest energy error over the run and the error at its end.
fn energy_errors(scheme: FixedStepScheme) -> (f64, f64) {
    let points = endpoints(&Oscillator::new(vec![0]), 100.0, config(0.01, scheme))
        .expect("the oscillator integrates");
    assert_eq!(points.len(), 10_000);
    let errors: Vec<f64> = points
        .iter()
        .map(|(state, _, _)| (energy(state) - 0.5).abs())
        .collect();
    (
        errors.iter().copied().fold(0.0, f64::max),
        *errors.last().expect("at least one step"),
    )
}

/// The standard symplectic witness: over 10,000 steps of `h = 0.01` explicit
/// Euler multiplies the energy by `(1 + h^2)` per step, so it ends near
/// `0.5 e`, while the partitioned step keeps it within `O(h)` of `0.5`.
#[test]
fn semi_implicit_energy_error_stays_bounded_where_explicit_euler_grows() {
    let (explicit_max, explicit_end) = energy_errors(FixedStepScheme::Explicit);
    let (semi_max, semi_end) = energy_errors(FixedStepScheme::SemiImplicit);
    let growth = 0.5 * (1.0_f64 + 1.0e-4).powi(10_000) - 0.5;
    assert!(
        (explicit_end - growth).abs() < 1.0e-9,
        "{explicit_end} vs {growth}"
    );
    assert_eq!(explicit_max, explicit_end);
    assert!(semi_max < 0.003, "{semi_max}");
    assert!(semi_end <= semi_max);
}

#[test]
fn semi_implicit_step_halving_converges_at_first_order() {
    let errors: Vec<f64> = [16.0, 32.0, 64.0, 128.0, 256.0]
        .iter()
        .map(|steps: &f64| {
            let points = endpoints(
                &Oscillator::new(vec![0]),
                1.0,
                config(1.0 / steps, FixedStepScheme::SemiImplicit),
            )
            .expect("the oscillator integrates");
            let (state, _, _) = points.last().expect("at least one step");
            (state[0] - 1.0_f64.cos()).hypot(state[1] + 1.0_f64.sin())
        })
        .collect();
    for pair in errors.windows(2) {
        let observed = (pair[0] / pair[1]).log2();
        assert!((observed - 1.0).abs() < 0.25, "observed {observed:.3}");
    }
}

#[test]
fn each_semi_implicit_step_spends_one_full_and_one_selected_evaluation() {
    let system = Oscillator::new(vec![0]);
    let points = endpoints(&system, 1.0, config(0.125, FixedStepScheme::SemiImplicit))
        .expect("the oscillator integrates");
    let counts: Vec<(usize, usize)> = points
        .iter()
        .map(|(_, full, selected)| (*full, *selected))
        .collect();
    let expected: Vec<(usize, usize)> = (1..=8).map(|step| (step, step)).collect();
    assert_eq!(counts, expected);
    // One step by hand: `v` first, then `x` from the new `v`.
    let h = 0.125;
    let v = -h;
    let x = 1.0 + h * v;
    assert_eq!(points[0].0, [x, v]);
}

/// With no kinematic state, or no other state, the step is explicit Euler
/// bit for bit and spends one evaluation.
#[test]
fn an_empty_side_of_the_partition_is_explicit_euler_bit_for_bit() {
    let reference = endpoints(
        &Oscillator::new(vec![0]),
        1.0,
        config(0.1, FixedStepScheme::Explicit),
    )
    .expect("the oscillator integrates");
    for kinematic in [vec![], vec![0, 1]] {
        let system = Oscillator::new(kinematic);
        let points = endpoints(&system, 1.0, config(0.1, FixedStepScheme::SemiImplicit))
            .expect("the oscillator integrates");
        let bits = |points: &[(Vec<f64>, usize, usize)]| -> Vec<Vec<u64>> {
            points
                .iter()
                .map(|(state, _, _)| state.iter().map(|value| value.to_bits()).collect())
                .collect()
        };
        assert_eq!(bits(&points), bits(&reference));
        assert_eq!(system.full.get(), points.len());
        assert_eq!(system.selected.get(), 0);
    }
}

#[test]
fn the_semi_implicit_scheme_is_refused_with_any_order_but_one() {
    for order in [2, 4] {
        let setup = MeNumericalSetup::new(1.0e-6, 1.0e-9, vec![1.0; 2], 2, None)
            .expect("fixture setup is checked");
        let error = fixed_step_integrator(
            setup,
            FixedStepConfig {
                step: 0.01,
                order,
                scheme: FixedStepScheme::SemiImplicit,
            },
        )
        .err()
        .expect("no plugin for a scheme the order does not admit");
        assert_eq!(
            error,
            BackendOptionsError::InvalidValue {
                backend: NumericalBackend::FixedStep,
                field: "scheme",
            }
        );
    }
}

/// The step of a plugin that asks for kinematic derivatives at the step's
/// start, before any full evaluation of the step or, with `after_full`, right
/// after one there, and then returns the opaque refusal: the host answers
/// with the typed cause, ahead of whatever the plugin returned.
fn eager_kinematic(after_full: bool) -> StepHook {
    Box::new(
        move |derivatives: &MeDerivativeHandle, request: &MeAdvanceRequest| {
            let start = request.current();
            let mut out = vec![0.0; 2];
            if after_full {
                derivatives.derivatives_into(start.time(), start.states(), &mut out);
                let full: Vec<u64> = out.iter().map(|value| value.to_bits()).collect();
                derivatives.kinematic_derivatives_into(start.time(), start.states(), &mut out);
                // No kinematic entry to poison: the refusal writes nothing.
                assert_eq!(
                    out.iter().map(|value| value.to_bits()).collect::<Vec<_>>(),
                    full
                );
            } else {
                derivatives.kinematic_derivatives_into(start.time(), start.states(), &mut out);
                assert!(out[0].is_nan());
            }
            Err(MeIntegrationError::DerivativeRefused)
        },
    )
}

/// The plugin that steps by `hook`, wrapping the semi-implicit fixed-step
/// plugin it withholds the derivative capability from.
fn eager(setup: MeNumericalSetup, after_full: bool) -> Box<dyn MeIntegratorBackend> {
    let inner = fixed_step_integrator(setup, config(0.1, FixedStepScheme::SemiImplicit))
        .expect("a valid configuration");
    Box::new(Shared::stepping_with(inner, eager_kinematic(after_full)))
}

#[test]
fn a_kinematic_request_without_a_current_full_evaluation_is_refused() {
    let source: Rc<dyn ContinuousOde> = Oscillator::new(vec![0]);
    let outcome = integrate_ode(
        source,
        &run(1.0),
        &|setup| eager(setup, false),
        |_interval| Ok(()),
    );
    assert!(
        matches!(
            outcome,
            Err(OdeDriveError::Integration(
                MeIntegrationError::NoEvaluatedPoint { .. }
            ))
        ),
        "{outcome:?}"
    );
}

/// `x' = v`, `v' = -x` declaring nothing about kinematic states: its
/// partition is the `ContinuousOde` default.
#[derive(Default)]
struct Undeclared {
    full: Cell<usize>,
}

impl ContinuousOde for Undeclared {
    fn width(&self) -> usize {
        2
    }

    fn derivatives_into(&self, _time: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        self.full.set(self.full.get() + 1);
        out[0] = state[1];
        out[1] = -state[0];
        Ok(())
    }
}

/// A system that declares no kinematic states has the empty default
/// partition, so the semi-implicit scheme takes explicit Euler steps bit for
/// bit at one evaluation each and never asks for a kinematic derivative.
#[test]
fn a_system_without_a_declared_partition_steps_as_explicit_euler() {
    let drive = |scheme| {
        let system = Rc::new(Undeclared::default());
        let source: Rc<dyn ContinuousOde> = Rc::clone(&system) as Rc<dyn ContinuousOde>;
        let mut ends = Vec::new();
        integrate_ode(
            source,
            &run(1.0),
            &|setup| {
                fixed_step_integrator(setup, config(0.125, scheme)).expect("a valid configuration")
            },
            |interval| {
                ends.push(
                    interval
                        .end_state()
                        .iter()
                        .map(|value| value.to_bits())
                        .collect::<Vec<_>>(),
                );
                Ok(())
            },
        )
        .expect("the oscillator integrates");
        (ends, system.full.get())
    };
    let (explicit, explicit_evaluations) = drive(FixedStepScheme::Explicit);
    let (semi_implicit, semi_implicit_evaluations) = drive(FixedStepScheme::SemiImplicit);
    assert_eq!(explicit.len(), 8);
    assert_eq!(semi_implicit, explicit);
    assert_eq!(explicit_evaluations, 8);
    assert_eq!(semi_implicit_evaluations, explicit_evaluations);
}

/// A kinematic request to a system that declares no kinematic states, even
/// right after a full evaluation at the same point, is the system's own typed
/// refusal rather than any value.
#[test]
fn a_kinematic_request_to_a_system_without_kinematic_states_is_refused() {
    let source: Rc<dyn ContinuousOde> = Rc::new(Undeclared::default());
    let outcome = integrate_ode(
        source,
        &run(1.0),
        &|setup| eager(setup, true),
        |_interval| Ok(()),
    );
    assert!(
        matches!(
            &outcome,
            Err(OdeDriveError::Integration(MeIntegrationError::Component(
                rumoca_solver::fmi_me::MeError::Evaluation { message }
            ))) if message == "the system has no kinematic states"
        ),
        "{outcome:?}"
    );
}
