//! Manufactured-solution convergence of the `velimplicit` plugin through the
//! canonical batch host, with its real directional-derivative Jacobian.
//!
//! The only hard horizon is the stop time t = 1; the outputs at t = 1/3 and
//! 2/3 are soft observations, so they fall strictly inside accepted steps
//! (asserted from the plugin's own sample calls) and observe its continuous
//! extension, while t = 1 observes a step endpoint. The error at each must
//! fall at least at the method's first order as the fixed step is halved.
//! Where the derivative takes equal values at both ends of [0, 1] (the
//! inflection and equal-ends cases at t = 1) the leading Riemann-sum error
//! cancels and second order is the expected outcome, so the bound is [1.6, 4.5].
//! For the equal-ends case the step sum of cos(2 pi t) over the full period is
//! exactly zero, so x(1) is exact to roundoff; a pair of errors both at
//! roundoff level is accepted as exact rather than given a ratio.

use std::cell::{Cell, RefCell};

use rumoca_solver::{
    SimExecutionPolicy,
    fmi_me::{
        MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError,
        MeIntegratorBackend, MeStepCandidate, driver::batch_output_cursor,
        session::MeRetainedComponent,
    },
};
use rumoca_solver_velimplicit::{VelImplicitConfig, model_exchange_integrator};

use super::{SimOptions, SimSolverMode, compile};

/// A model with one state `x` and its exact solution.
struct Case {
    name: &'static str,
    source: &'static str,
    exact: fn(f64) -> f64,
}

const CASES: [Case; 4] = [
    Case {
        name: "Curved",
        source: "model Curved Real x(start = 1, fixed = true); equation der(x) = x; end Curved;",
        exact: f64::exp,
    },
    Case {
        // x = s^3 - s with s = t - 0.5: an inflection inside the interval.
        name: "Inflection",
        source: "model Inflection Real x(start = 0.375, fixed = true); \
                 equation der(x) = 3 * (time - 0.5)^2 - 1; end Inflection;",
        exact: |t| (t - 0.5).powi(3) - (t - 0.5),
    },
    Case {
        // Equal derivatives (2 pi) at both ends of [0, 1].
        name: "EqualEnds",
        source: "model EqualEnds Real x(start = 0, fixed = true); \
                 equation der(x) = 6.283185307179586 * cos(6.283185307179586 * time); \
                 end EqualEnds;",
        exact: |t| (2.0 * std::f64::consts::PI * t).sin(),
    },
    Case {
        // Stiff relaxation onto cos(t): exercises the implicit Jacobian.
        name: "Relaxation",
        source: "model Relaxation Real x(start = 1, fixed = true); \
                 equation der(x) = -50 * (x - cos(time)) - sin(time); end Relaxation;",
        exact: f64::cos,
    },
];

/// The soft output observations (1/3, 2/3) and the stop time.
const TIMES: [f64; 3] = [1.0 / 3.0, 2.0 / 3.0, 1.0];

/// Forwards to the plugin and records, for every sample, the time and the
/// accepted interval it was drawn from.
struct SampleRecorder<'a> {
    inner: Box<dyn MeIntegratorBackend>,
    interval: RefCell<(f64, f64)>,
    samples: &'a RefCell<Vec<(f64, f64, f64)>>,
    truncations: &'a Cell<usize>,
}

impl MeIntegratorBackend for SampleRecorder<'_> {
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        self.inner.initialize(point, derivatives)
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let candidate = self.inner.advance(request)?;
        *self.interval.borrow_mut() = (request.current().time(), candidate.accepted_time());
        Ok(candidate)
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        let (start, end) = *self.interval.borrow();
        self.samples.borrow_mut().push((time, start, end));
        self.inner.sample(time, states)
    }

    fn continuous_extension_order(&self) -> Option<u32> {
        self.inner.continuous_extension_order()
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        self.truncations.set(self.truncations.get() + 1);
        self.inner.truncate_reset(point)
    }
}

/// `x` at [`TIMES`] with a fixed step `h` and Jacobian reuse `reuse`, plus
/// the recorded samples.
fn run(case: &Case, h: f64, reuse: u32) -> (Vec<f64>, Vec<(f64, f64, f64)>) {
    let truncations = Cell::new(0);
    let (result, samples) = run_recorded(
        case.source,
        case.name,
        (1.0, 1.0 / 3.0),
        (h, reuse),
        &truncations,
    );
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state is recorded");
    let values = TIMES
        .iter()
        .map(|&time| {
            let row = result
                .times
                .iter()
                .rposition(|recorded| (recorded - time).abs() < 1.0e-12)
                .expect("output time is recorded");
            result.data[x][row]
        })
        .collect();
    (values, samples)
}

/// One batch run of `source` through the recording wrapper, with stop time
/// and output interval `(t_end, dt)` and step and reuse `(h, reuse)`.
fn run_recorded(
    source: &str,
    name: &str,
    (t_end, dt): (f64, f64),
    (h, reuse): (f64, u32),
    truncations: &Cell<usize>,
) -> (rumoca_solver::SimResult, Vec<(f64, f64, f64)>) {
    let opts = SimOptions {
        execution_policy: SimExecutionPolicy::Interpreter,
        solver_mode: SimSolverMode::VelImplicit,
        t_end,
        dt: Some(dt),
        ..SimOptions::default()
    };
    let dae = compile(source, name);
    let (artifact, backend) = super::super::lower_runtime_fmi_artifact(&dae, &opts).unwrap();
    let backend =
        rumoca_solver::fmi_me::admit_execution_backend(opts.execution_policy, backend).unwrap();
    let options = crate::me_backend::batch_options(&artifact.root_location(), &opts).unwrap();
    let mut cursor = batch_output_cursor(&options).unwrap();
    let retained = MeRetainedComponent::instantiate(
        artifact.source(),
        &crate::me_backend::instance_config("velimplicit-convergence", &opts).unwrap(),
        backend,
    )
    .unwrap();
    let host = retained.into_lease(options).unwrap();
    let setup = host.numerical_setup(Some(h)).unwrap();
    let samples = RefCell::new(Vec::new());
    let plugin = SampleRecorder {
        inner: model_exchange_integrator(
            setup,
            VelImplicitConfig {
                step: h,
                jacobian_reuse: reuse,
            },
        ),
        interval: RefCell::new((0.0, 0.0)),
        samples: &samples,
        truncations,
    };
    // The wrapper declares the plugin's own continuous-extension order.
    assert_eq!(plugin.continuous_extension_order(), Some(1));
    let mut session = host.into_session(Some(Box::new(plugin))).unwrap();
    session.run_to_stop(&mut cursor).unwrap();
    (session.finish(), samples.into_inner())
}

/// A located state event reaches the plugin as a truncate/reset through the
/// wrapper (the bouncing ball of `velimplicit_batch`).
#[test]
fn a_state_event_truncates_the_recorded_plugin() {
    let source = "model Ball
                    Real h(start = 1, fixed = true);
                    Real v(start = 0, fixed = true);
                  equation
                    der(h) = v;
                    der(v) = -9.81;
                    when h < 0 then
                      reinit(v, -0.8 * pre(v));
                    end when;
                  end Ball;";
    let truncations = Cell::new(0);
    let (result, _) = run_recorded(source, "Ball", (0.6, 0.1), (1.0e-3, 200), &truncations);
    assert!(
        truncations.get() >= 1,
        "no truncate/reset reached the plugin"
    );
    assert!((result.times[result.times.len() - 1] - 0.6).abs() < 1.0e-12);
}

/// Errors at or below this are roundoff, not discretization error.
const ROUNDOFF: f64 = 1.0e-12;

/// Problems with one case at one Jacobian reuse: soft observations that were
/// not interior samples, and error ratios outside [1.6, 4.5].
fn case_failures(case: &Case, reuse: u32, steps: &[f64]) -> Vec<String> {
    let mut failures = Vec::new();
    let mut errors = Vec::new();
    for &h in steps {
        let (values, samples) = run(case, h, reuse);
        for &soft in &TIMES[..2] {
            let interior = samples.iter().any(|&(time, start, end)| {
                (time - soft).abs() < 1.0e-12 && start < time && time < end
            });
            if !interior {
                failures.push(format!(
                    "{} reuse {reuse} h={h}: t={soft:.4} was not sampled strictly inside an \
                     accepted step",
                    case.name
                ));
            }
        }
        errors.push(
            values
                .iter()
                .zip(TIMES)
                .map(|(value, time)| (value - (case.exact)(time)).abs())
                .collect::<Vec<f64>>(),
        );
    }
    for (column, time) in TIMES.iter().enumerate() {
        for pair in errors.windows(2) {
            let ratio = pair[0][column] / pair[1][column];
            let exact = pair[0][column] <= ROUNDOFF && pair[1][column] <= ROUNDOFF;
            if !exact && !(1.6..=4.5).contains(&ratio) {
                failures.push(format!(
                    "{} reuse {reuse} t={time:.4}: error ratio {ratio:.3} \
                     ({:.3e} -> {:.3e}) is not at least first order",
                    case.name, pair[0][column], pair[1][column]
                ));
            }
        }
    }
    failures
}

#[test]
fn velimplicit_converges_at_first_order_on_manufactured_solutions() {
    // 1/3 and 2/3 are never boundaries of steps 1/40, ..., 1/320 from t = 0.
    // The coarsest step keeps the stiff case (rate 50) out of its
    // pre-asymptotic range.
    let steps = [1.0 / 40.0, 1.0 / 80.0, 1.0 / 160.0, 1.0 / 320.0];
    let failures: Vec<String> = CASES
        .iter()
        .flat_map(|case| [1, 7].map(|reuse| case_failures(case, reuse, &steps)))
        .flatten()
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
