//! A changed input that reaches no event owner stays in Continuous-Time Mode
//! and leaves the session bit for bit where the input event would.

use rumoca_solver::fmi_me::{
    MeInstanceConfig, MeIntegratorBackend, MeNumericalSetup,
    driver::{advance_live_session, live_session_options},
    session::{
        MeRetainedComponent,
        input_write_audit::{InputWriteAudit, force_input_event, input_write_counts},
    },
};

use super::{SimOptions, SimSolverMode, compile};

#[derive(Clone, Copy, Debug)]
enum Step {
    Set(&'static [(&'static str, f64)]),
    Advance(f64),
}

/// What one run of a script leaves behind.
#[derive(Debug, PartialEq)]
struct Run {
    reach: Vec<bool>,
    audits: Vec<InputWriteAudit>,
    trace: String,
    /// Changed batches as `[continuous, input event]`.
    writes: [usize; 2],
}

fn plugin(solver_mode: SimSolverMode, setup: MeNumericalSetup) -> Box<dyn MeIntegratorBackend> {
    match solver_mode {
        SimSolverMode::Bdf => rumoca_solver_diffsol::model_exchange_integrator(setup),
        _ => rumoca_solver_rk45::model_exchange_integrator(setup),
    }
}

fn run(
    source: &str,
    model: &str,
    solver_mode: SimSolverMode,
    forced: bool,
    script: &[Step],
) -> Run {
    let opts = SimOptions {
        solver_mode,
        t_end: 1.0,
        dt: Some(0.1),
        atol: 1e-9,
        rtol: 1e-9,
        ..SimOptions::default()
    };
    let dae = compile(source, model);
    let (artifact, backend) = super::super::lower_runtime_fmi_artifact(&dae, &opts)
        .expect("fixture lowers to a component");
    let backend =
        rumoca_solver::fmi_me::admit_execution_backend(opts.execution_policy, backend).unwrap();
    let config = MeInstanceConfig::open_ended("continuous-input-write", opts.rtol, opts.t_start)
        .unwrap()
        .with_model_state_jacobian_request(match solver_mode {
            SimSolverMode::Bdf => rumoca_solver::ModelStateJacobianRequest::FiniteAssembly,
            _ => rumoca_solver::ModelStateJacobianRequest::NotRequested,
        });
    let retained = MeRetainedComponent::instantiate(artifact.source(), &config, backend).unwrap();
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
    let setup = host
        .numerical_setup(Some(crate::me_backend::default_step_size(&opts)))
        .unwrap();
    let mut session = host.into_session(Some(plugin(solver_mode, setup))).unwrap();
    force_input_event(forced);
    let before = input_write_counts();
    let reach = session.input_event_reach().unwrap();
    let mut audits = vec![session.input_write_audit().unwrap()];
    for step in script {
        match *step {
            Step::Set(inputs) => session.set_inputs(inputs).unwrap(),
            Step::Advance(time) => {
                advance_live_session(&mut session, time).unwrap();
            }
        }
        audits.push(
            session
                .input_write_audit()
                .unwrap_or_else(|error| panic!("{model} {solver_mode:?} after {step:?}: {error}")),
        );
    }
    let after = input_write_counts();
    force_input_event(false);
    Run {
        reach,
        audits,
        trace: format!("{:?}", session.finish()),
        writes: [after[0] - before[0], after[1] - before[1]],
    }
}

/// Run `script` with the continuous path and with every changed batch forced
/// through the input event; both must leave identical bits after every step.
/// Returns the continuous run's reach, write counts and the number of
/// batches written while the session stood on a deferred yield.
fn assert_paths_agree(
    source: &str,
    model: &str,
    script: &[Step],
) -> Vec<(SimSolverMode, Vec<bool>, [usize; 2], usize)> {
    [SimSolverMode::RkLike, SimSolverMode::Bdf]
        .into_iter()
        .map(|solver_mode| {
            let continuous = run(source, model, solver_mode, false, script);
            let event = run(source, model, solver_mode, true, script);
            assert_eq!(continuous.reach, event.reach);
            assert_eq!(event.writes[0], 0, "{model} {solver_mode:?}: forced run");
            assert_eq!(
                continuous.writes[0] + continuous.writes[1],
                event.writes[1],
                "{model} {solver_mode:?}: both runs see the same changed batches"
            );
            for (index, (plain, evented)) in continuous.audits.iter().zip(&event.audits).enumerate()
            {
                assert_eq!(
                    plain,
                    evented,
                    "{model} {solver_mode:?}: first difference after step {index} ({:?})",
                    index.checked_sub(1).map(|step| script[step])
                );
            }
            assert_eq!(
                continuous.trace, event.trace,
                "{model} {solver_mode:?}: trace"
            );
            let deferred = script
                .iter()
                .zip(&continuous.audits)
                .filter(|(step, before)| matches!(step, Step::Set(_)) && before.deferred_yield)
                .count();
            (solver_mode, continuous.reach, continuous.writes, deferred)
        })
        .collect()
}

/// The issued reach of each declared input of `model`, in declaration order.
fn reach(source: &str, model: &str) -> Vec<bool> {
    run(source, model, SimSolverMode::RkLike, false, &[]).reach
}

#[test]
fn input_event_reach_follows_event_owners_through_algebraic_definitions() {
    let mut mismatches = Vec::new();
    for (source, model, expected) in [
        (
            "model DerivativeOnly input Real u = 0; Real x(start=0, fixed=true); \
             equation der(x) = u; end DerivativeOnly;",
            "DerivativeOnly",
            vec![false],
        ),
        (
            "model TwoSteps input Real u = 0; input Real w = 0; Real x(start=0, fixed=true); \
             Real a; Real b; output Real y; \
             equation a = 2*u; b = a + 1; y = if b > 0 then x else -x; der(x) = w; end TwoSteps;",
            "TwoSteps",
            vec![true, false],
        ),
        (
            "model WhenCondition input Real u = 0; input Real w = 0; Real x(start=0, fixed=true); \
             discrete Real d(start=0, fixed=true); \
             equation der(x) = w; when u > 0.5 then d = pre(d) + 1; end when; end WhenCondition;",
            "WhenCondition",
            vec![true, false],
        ),
        (
            "model WhenBody input Real u = 0; input Real w = 1; Real x(start=0, fixed=true); \
             discrete Real d(start=0, fixed=true); \
             equation der(x) = w; when x > 0.5 then d = u; end when; end WhenBody;",
            "WhenBody",
            vec![true, false],
        ),
        (
            "model Reinit input Real u = 0; input Real w = 1; Real x(start=0, fixed=true); \
             equation der(x) = w; when x > 0.5 then reinit(x, u); end when; end Reinit;",
            "Reinit",
            vec![true, false],
        ),
        (
            "model PreCopy input Real u = 0; input Real w = 1; Real x(start=0, fixed=true); \
             discrete Real d(start=0, fixed=true); discrete Real e(start=0, fixed=true); \
             equation der(x) = w; when x > 0.5 then d = pre(d) + u; end when; \
             when x > 0.75 then e = pre(e) + 1; end when; end PreCopy;",
            "PreCopy",
            vec![true, false],
        ),
        (
            "model PreFree input Real u = 0; Real x(start=0, fixed=true); \
             discrete Real e(start=0, fixed=true); \
             equation der(x) = 1 + u; when x > 0.75 then e = pre(e) + 1; end when; end PreFree;",
            "PreFree",
            vec![false],
        ),
        (
            "model BooleanCopy input Boolean on = false; input Real w = 1; \
             Real x(start=0, fixed=true); Boolean active; \
             equation der(x) = w; active = on; end BooleanCopy;",
            "BooleanCopy",
            vec![true, false],
        ),
        (
            "model LoopRelation input Real u = 0; input Real w = 1; Real x(start=0, fixed=true); \
             Real a; Real b; output Real y; \
             equation a = u - 0.1*sin(b); b = a + 0.1*cos(a); y = if b > 0 then x else -x; \
             der(x) = w; end LoopRelation;",
            "LoopRelation",
            vec![true, true],
        ),
    ] {
        let actual = reach(source, model);
        if actual != expected {
            mismatches.push((model, actual, expected));
        }
    }
    assert!(
        mismatches.is_empty(),
        "(model, issued, expected): {mismatches:?}"
    );
}

const NO_EVENTS: &str = "model NoEvents input Real u = 1; input Real w = 0; \
    Real x(start=1, fixed=true); Real z; output Real y; \
    equation z = 2*u + w; der(x) = -x + z; y = x + z; end NoEvents;";

#[test]
fn a_model_without_events_writes_inputs_continuously() {
    let script = [
        Step::Advance(0.1),
        Step::Set(&[("u", 2.0)]),
        Step::Advance(0.2),
        Step::Set(&[("u", 2.0), ("w", -1.0)]),
        Step::Set(&[("w", 0.5)]),
        // Off the output grid: the yield may be deferred inside a step.
        Step::Advance(0.233),
        Step::Set(&[("u", -3.0)]),
        Step::Advance(0.31),
        Step::Set(&[("u", 0.0), ("u", 4.0)]),
        Step::Advance(0.5),
    ];
    for (solver_mode, reach, writes, deferred) in assert_paths_agree(NO_EVENTS, "NoEvents", &script)
    {
        assert_eq!(reach, [false, false]);
        assert_eq!(writes, [5, 0], "{solver_mode:?}");
        assert!(
            deferred > 0,
            "{solver_mode:?}: no write met a deferred yield"
        );
    }
}

const STATE_RELATIONS: &str = "model StateRelations input Real u = 0; \
    Real x(start=0, fixed=true); Real v(start=1, fixed=true); \
    discrete Real hits(start=0, fixed=true); output Real y; \
    equation der(x) = u; der(v) = -1; y = if x > 0 then 1 else -1; \
    when v < 0.75 then hits = pre(hits) + 1; end when; end StateRelations;";

#[test]
fn state_relations_at_zero_and_inside_tolerance_keep_the_input_event_bits() {
    let script = [
        Step::Advance(0.1),
        // `x > 0` sits exactly at zero.
        Step::Set(&[("u", 1e-13)]),
        Step::Advance(0.2),
        // `x` is 1e-14: inside the location tolerance of `x > 0`.
        Step::Set(&[("u", -1e-13)]),
        Step::Advance(0.3),
        Step::Set(&[("u", 1.0)]),
        Step::Advance(0.4),
        Step::Set(&[("u", -1.0)]),
        Step::Advance(0.6),
    ];
    for (solver_mode, reach, writes, _) in
        assert_paths_agree(STATE_RELATIONS, "StateRelations", &script)
    {
        assert_eq!(reach, [false]);
        assert_eq!(writes, [4, 0], "{solver_mode:?}");
    }
}

const TICKS: &str = "model Ticks input Real u = 0; Real x(start=0, fixed=true); \
    discrete Real s(start=0, fixed=true); \
    equation der(x) = u; when sample(0, 0.1) then s = pre(s) + x; end when; end Ticks;";

#[test]
fn a_write_on_a_periodic_tick_keeps_the_input_event_bits() {
    let script = [
        Step::Advance(0.1),
        Step::Set(&[("u", 1.0)]),
        Step::Advance(0.15),
        Step::Set(&[("u", 2.0)]),
        Step::Advance(0.2),
        Step::Set(&[("u", -1.0)]),
        Step::Advance(0.35),
    ];
    for (solver_mode, reach, writes, _) in assert_paths_agree(TICKS, "Ticks", &script) {
        assert_eq!(reach, [false]);
        // The ticks at 0.1 and 0.2 are still due when those writes arrive.
        assert_eq!(writes, [1, 2], "{solver_mode:?}");
    }
}

const MIXED: &str = "model Mixed input Real u = 0; input Real g = 1; \
    Real x(start=0, fixed=true); output Real y; \
    equation der(x) = u; y = if g > 0.5 then x else -x; end Mixed;";

#[test]
fn an_identically_rewritten_reached_input_does_not_force_the_input_event() {
    let script = [
        Step::Advance(0.1),
        // `g` reaches a relation but keeps its bits; only `u` changes.
        Step::Set(&[("u", 2.0), ("g", 1.0)]),
        Step::Advance(0.2),
        Step::Set(&[("g", 0.0)]),
        Step::Advance(0.3),
        Step::Set(&[("g", 0.0), ("u", -1.0)]),
        Step::Advance(0.4),
    ];
    for (solver_mode, reach, writes, _) in assert_paths_agree(MIXED, "Mixed", &script) {
        assert_eq!(reach, [false, true]);
        assert_eq!(writes, [2, 1], "{solver_mode:?}");
    }
}
