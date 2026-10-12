//! Observe the canonical host's actual RK endpoint, sampling, and root adoption.

use std::cell::{Cell, RefCell};

use rumoca_solver::{
    SimExecutionPolicy,
    fmi_me::{
        MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeInstanceConfig,
        MeIntegrationError, MeIntegratorBackend, MeStepCandidate,
        driver::{advance_live_session, live_session_options},
        session::MeRetainedComponent,
    },
};

use super::{SimOptions, SimSolverMode, compile};

const SOURCE: &str = "model ScalarRealQuotient output Real y; Real x(start=0, fixed=true); equation y=mod(8*time+2,7); der(x)=y; end ScalarRealQuotient;";
const ROOT: f64 = 0.625;

#[test]
fn quotient_wire_replay_requires_the_issued_live_root() {
    let dae = compile(SOURCE, "ScalarRealQuotient");
    let solve = super::lower_dae_for_simulation(&dae, &SimOptions::default()).unwrap();
    let mut wire = serde_json::to_value(&solve.problem).unwrap();
    serde_json::from_value::<rumoca_ir_solve::SolveProblem>(wire.clone()).unwrap();
    let roles = wire["discrete"]["row_roles"].as_array_mut().unwrap();
    let memory = roles
        .iter_mut()
        .find(|role| role.get("QuotientMemory").is_some())
        .unwrap();
    memory["QuotientMemory"]["root_index"] = serde_json::json!(usize::MAX);
    let error = serde_json::from_value::<rumoca_ir_solve::SolveProblem>(wire).unwrap_err();
    assert!(error.to_string().contains("live root owner"), "{error}");
}

#[test]
fn quotient_memory_uses_solved_initialization_and_signed_rounding() {
    let cases = [
        ("mod", "15", "1"),
        ("mod", "-15", "6"),
        ("rem", "-15", "-1"),
    ];
    for (builtin, initial, expected) in cases {
        let source = format!(
            "model InitializedQuotient parameter Real a(start=99,fixed=false); Real x(start=0,fixed=true); output Real y; initial equation a={initial}; equation y={builtin}(8*time+a,7); der(x)=y; end InitializedQuotient;"
        );
        let dae = compile(&source, "InitializedQuotient");
        let expected: f64 = expected.parse().unwrap();
        for execution_policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
            let opts = SimOptions {
                execution_policy,
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.1,
                dt: Some(0.1),
                atol: 1e-9,
                rtol: 1e-9,
                ..SimOptions::default()
            };
            let result = super::simulate_dae(&dae, &opts).unwrap();
            let y = result.names.iter().position(|name| name == "y").unwrap();
            let x = result.names.iter().position(|name| name == "x").unwrap();
            assert!(
                (result.data[y][0] - expected).abs() < 1e-9,
                "{builtin}({initial},7): {:?}",
                result.data[y]
            );
            let integral = expected * 0.1 + 4.0 * 0.1_f64.powi(2);
            assert!(
                (result.data[x].last().unwrap() - integral).abs() < 1e-8,
                "{builtin}({initial},7): {:?}",
                result.data[x]
            );
        }
    }
}

struct Observation {
    kind: &'static str,
    start: f64,
    end: f64,
    left: Vec<f64>,
    right: Vec<f64>,
}

struct RecordingIntegrator<'a> {
    inner: Box<dyn MeIntegratorBackend>,
    observations: &'a RefCell<Vec<Observation>>,
    interval: Cell<Option<(f64, f64)>>,
}

impl RecordingIntegrator<'_> {
    fn record(&self, kind: &'static str, start: f64, end: f64, left: &[f64], right: &[f64]) {
        let mut observations = self.observations.borrow_mut();
        assert!(
            observations.len() < 4096,
            "bounded root diagnostic exceeded"
        );
        observations.push(Observation {
            kind,
            start,
            end,
            left: left.to_vec(),
            right: right.to_vec(),
        });
    }
}

impl MeIntegratorBackend for RecordingIntegrator<'_> {
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        self.interval.set(None);
        self.inner.initialize(point, derivatives)
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let candidate = self.inner.advance(request)?;
        self.interval
            .set(Some((request.current().time(), candidate.accepted_time())));
        if near_root(request.current().time(), candidate.accepted_time()) {
            self.record(
                "candidate",
                request.current().time(),
                candidate.accepted_time(),
                request.current().states(),
                candidate.accepted_states(),
            );
        }
        Ok(candidate)
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        self.inner.sample(time, states)?;
        // The delegate neither evaluates the component nor adds sample calls.
        if self
            .interval
            .get()
            .is_some_and(|(start, end)| near_root(start, end))
        {
            self.record("dense sample", time, time, states, states);
        }
        Ok(())
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        if near_root(point.time(), point.time()) {
            self.record(
                "root adoption",
                point.time(),
                point.time(),
                point.states(),
                point.states(),
            );
        }
        self.inner.truncate_reset(point)?;
        self.interval.set(None);
        Ok(())
    }
}

fn near_root(start: f64, end: f64) -> bool {
    start <= ROOT + 0.01 && end >= ROOT - 0.01
}

fn expected_integral(time: f64) -> f64 {
    4.0 * time * time + 2.0 * time - 7.0 * (time - ROOT).max(0.0)
}

fn check_value(failures: &mut Vec<String>, label: &str, time: f64, actual: f64, opts: &SimOptions) {
    let expected = expected_integral(time);
    let bound = 20.0 * (opts.atol + opts.rtol * expected.abs());
    if !actual.is_finite() || (actual - expected).abs() > bound {
        failures.push(format!(
            "{label}: x({time:.17})={actual:.17}, expected={expected:.17}, bound={bound:.17}"
        ));
    }
}

fn check_observations(observations: &[Observation], failures: &mut Vec<String>, opts: &SimOptions) {
    let root = observations
        .iter()
        .find(|entry| entry.kind == "root adoption" && (entry.start - ROOT).abs() < 1e-6)
        .expect("actual host adopts the scalar quotient root");
    assert!(
        observations.iter().any(|entry| {
            entry.kind == "candidate" && entry.start <= root.start && root.start <= entry.end
        }),
        "recorded numerical candidate contains the adopted root"
    );
    for entry in observations {
        assert_eq!(entry.left.len(), 1);
        assert_eq!(entry.right.len(), 1);
        assert!(entry.start.is_finite() && entry.end.is_finite() && entry.start <= entry.end);
        assert!(entry.left[0].is_finite() && entry.right[0].is_finite());
        eprintln!(
            "{}: [{:.17}, {:.17}], x=[{:.17}, {:.17}], expected=[{:.17}, {:.17}]",
            entry.kind,
            entry.start,
            entry.end,
            entry.left[0],
            entry.right[0],
            expected_integral(entry.start),
            expected_integral(entry.end)
        );
    }
    // Candidate endpoints after a discontinuity can be discarded by the host;
    // the physical accuracy oracle applies to the state it actually adopts.
    check_value(failures, "root adoption", root.start, root.right[0], opts);
}

fn capture_policy(policy: SimExecutionPolicy, failures: &mut Vec<String>) {
    let opts = SimOptions {
        execution_policy: policy,
        solver_mode: SimSolverMode::RkLike,
        t_end: 1.0,
        dt: Some(0.1),
        atol: 1e-9,
        rtol: 1e-9,
        ..SimOptions::default()
    };
    let dae = compile(SOURCE, "ScalarRealQuotient");
    let (artifact, backend) = super::super::lower_runtime_fmi_artifact(&dae, &opts).unwrap();
    let backend = rumoca_solver::fmi_me::admit_execution_backend(policy, backend).unwrap();
    let retained = MeRetainedComponent::instantiate(
        artifact.source(),
        &MeInstanceConfig::open_ended("scalar-root-capture", opts.rtol, opts.t_start).unwrap(),
        backend.clone(),
    )
    .unwrap();
    let options = live_session_options(
        &artifact.root_location(),
        opts.t_start,
        opts.rtol,
        opts.atol,
        opts.t_end - opts.t_start,
        opts.max_wall_seconds,
    )
    .unwrap();
    let host = retained.into_lease(options).unwrap();
    assert_eq!(host.state_count(), 1);
    let setup = host
        .numerical_setup(Some(crate::me_backend::default_step_size(&opts)))
        .unwrap();
    let observations = RefCell::new(Vec::new());
    let plugin = RecordingIntegrator {
        inner: rumoca_solver_rk45::model_exchange_integrator(setup),
        observations: &observations,
        interval: Cell::new(None),
    };
    let mut session = host.into_session(Some(Box::new(plugin))).unwrap();
    for sample in 0..=10 {
        let time = f64::from(sample) / 10.0;
        if sample != 0 {
            advance_live_session(&mut session, time).unwrap();
        }
        assert_eq!(session.time(), time);
        let values = session.visible_values().unwrap();
        let expected_y = (8.0 * time + 2.0) % 7.0;
        assert!(
            (values["y"] - expected_y).abs() <= 20.0 * (opts.atol + opts.rtol * expected_y.abs())
        );
        check_value(
            failures,
            &format!("{policy:?} sample"),
            time,
            values["x"],
            &opts,
        );
    }
    eprintln!(
        "actual execution: {policy:?}, receipt={:?}, declined={:?}",
        artifact.execution_receipt(),
        backend.as_ref().map(|backend| backend.declined())
    );
    check_observations(&observations.borrow(), failures, &opts);
}

#[test]
fn scalar_modulo_root_adoption_preserves_the_integral() {
    let mut failures = Vec::new();
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        capture_policy(policy, &mut failures);
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
