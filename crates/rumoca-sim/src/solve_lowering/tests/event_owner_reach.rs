//! Inside one event update an owner is evaluated again only when a lane it
//! reads changed bits since its last evaluation in the same event; the
//! session is left bit for bit where the full update would leave it.

use rumoca_ir_solve::EventReachRefusal;
use rumoca_solver::fmi_me::{
    MeInstanceConfig, MeIntegratorBackend, MeNumericalSetup,
    driver::{advance_live_session, live_session_options},
    session::{MeRetainedComponent, input_write_audit::InputWriteAudit},
};
use rumoca_solver::runtime::event_owner_audit::{
    EventOwnerCounts, event_owner_counts, force_full_event_update,
};

use super::{SimOptions, SimSolverMode, compile};

/// What one run of a script leaves behind.
#[derive(Debug)]
struct Run {
    refusal: Option<EventReachRefusal>,
    audits: Vec<InputWriteAudit>,
    /// The error the run stopped at, if one did.
    failure: Option<String>,
    trace: String,
    counts: EventOwnerCounts,
}

fn plugin(solver_mode: SimSolverMode, setup: MeNumericalSetup) -> Box<dyn MeIntegratorBackend> {
    match solver_mode {
        SimSolverMode::Bdf => rumoca_solver_diffsol::model_exchange_integrator(setup),
        _ => rumoca_solver_rk45::model_exchange_integrator(setup),
    }
}

/// The FMI component of `dae`, optionally round-tripped through the canonical
/// component wire the precompiled runtime loads.
fn component(
    dae: &rumoca_ir_dae::Dae,
    opts: &SimOptions,
    decoded: bool,
) -> (
    rumoca_ir_solve::fmi::FmiComponent,
    Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) {
    let lowered = super::super::lower_correlated_for_simulation_with_overrides(dae, opts)
        .expect("fixture lowers");
    let backend =
        crate::native_execution::admitted_native_execution_backend(opts, lowered.model()).unwrap();
    let component =
        rumoca_phase_solve::fmi::finish_fmi_component(lowered).expect("fixture is a component");
    if !decoded {
        return (component, backend);
    }
    let root = component.runtime_view().retain_assertion_root();
    let wire = serde_json::to_value(
        rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root).expect("component wire"),
    )
    .unwrap();
    let decoded = rumoca_eval_solve::fmi_wire::deserialize_fmi_component(wire)
        .expect("the component wire replays");
    let backend = crate::native_execution::admitted_native_execution_backend(
        opts,
        decoded.runtime_view().retain_assertion_root().model(),
    )
    .unwrap();
    (decoded, backend)
}

fn run(
    source: &str,
    model: &str,
    solver_mode: SimSolverMode,
    forced: bool,
    decoded: bool,
    stops: &[f64],
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
    let (component, backend) = component(&dae, &opts, decoded);
    let artifact = rumoca_solver::fmi_me::MeModelArtifact::new(component);
    let backend =
        rumoca_solver::fmi_me::admit_execution_backend(opts.execution_policy, backend).unwrap();
    let config = MeInstanceConfig::open_ended("event-owner-reach", opts.rtol, opts.t_start)
        .unwrap()
        .with_model_state_jacobian_request(match solver_mode {
            SimSolverMode::Bdf => rumoca_solver::ModelStateJacobianRequest::FiniteAssembly,
            _ => rumoca_solver::ModelStateJacobianRequest::NotRequested,
        });
    force_full_event_update(forced);
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
    let plugin = (host.state_count() != 0).then(|| {
        let setup = host
            .numerical_setup(Some(crate::me_backend::default_step_size(&opts)))
            .unwrap();
        plugin(solver_mode, setup)
    });
    let mut session = host.into_session(plugin).unwrap();
    let refusal = session.event_owner_refusal().unwrap();
    let before = event_owner_counts();
    let mut audits = vec![session.input_write_audit().unwrap()];
    let mut failure = None;
    for stop in stops {
        if let Err(error) = advance_live_session(&mut session, *stop) {
            failure = Some(error.to_string());
            break;
        }
        audits.push(session.input_write_audit().unwrap());
    }
    let counts = event_owner_counts() - before;
    force_full_event_update(false);
    Run {
        refusal,
        audits,
        failure,
        trace: format!("{:?}", session.finish()),
        counts,
    }
}

const STOPS: [f64; 5] = [0.15, 0.35, 0.55, 0.75, 0.95];

/// Run `model` restricted and with every event update forced through the
/// full path, under RK45 and BDF; both must leave identical bits after every
/// stop, take the same passes and fail the same way. Returns the restricted
/// run of each solver.
fn assert_restricted_matches_full(
    source: &str,
    model: &str,
    decoded: bool,
    stops: &[f64],
) -> Vec<(SimSolverMode, Run)> {
    [SimSolverMode::RkLike, SimSolverMode::Bdf]
        .into_iter()
        .map(|solver_mode| {
            let full = run(source, model, solver_mode, true, decoded, stops);
            let restricted = run(source, model, solver_mode, false, decoded, stops);
            let case = format!("{model} {solver_mode:?}");
            assert_eq!(full.refusal, restricted.refusal, "{case}: refusal");
            assert_eq!(restricted.failure, full.failure, "{case}: failure");
            assert_eq!(restricted.audits.len(), full.audits.len(), "{case}: stops");
            for (index, (restricted, full)) in
                restricted.audits.iter().zip(&full.audits).enumerate()
            {
                assert_eq!(restricted, full, "{case}: first difference at stop {index}");
            }
            assert_eq!(restricted.trace, full.trace, "{case}: trace");
            let (r, f) = (restricted.counts, full.counts);
            // The pass structure is steered by the same changed flags.
            assert_eq!(
                (
                    r.events,
                    r.outer_passes,
                    r.relation_refreshes,
                    r.post_commit_relation_refreshes,
                    r.discrete_passes,
                    r.condition_memory_passes,
                    r.action_evaluations,
                ),
                (
                    f.events,
                    f.outer_passes,
                    f.relation_refreshes,
                    f.post_commit_relation_refreshes,
                    f.discrete_passes,
                    f.condition_memory_passes,
                    f.action_evaluations,
                ),
                "{case}: passes"
            );
            // Every evaluation the restricted run skipped, the full run made.
            assert_eq!(
                (f.root_evaluations_skipped, f.discrete_programs_skipped),
                (0, 0),
                "{case}: the forced run skips nothing"
            );
            assert_eq!(
                r.root_evaluations + r.root_evaluations_skipped,
                f.root_evaluations,
                "{case}: root evaluations"
            );
            assert_eq!(
                r.discrete_programs_evaluated + r.discrete_programs_skipped,
                f.discrete_programs_evaluated,
                "{case}: discrete programs"
            );
            (solver_mode, restricted)
        })
        .collect()
}

/// A sampled controller over a plant with one relation and one unclocked
/// discrete row: the shape of one plant tick.
const TICKED_PLANT: &str = "model TickedPlant \
    Real x(start=1, fixed=true); Real v(start=0, fixed=true); \
    discrete Real u(start=0, fixed=true); Real f; Boolean near; \
    equation der(x) = v; der(v) = u - f; \
    f = if x < 0 then 100*x else 0; near = x < 0.2; \
    when sample(0, 0.1) then u = -2*x - 0.5*v; end when; end TickedPlant;";

#[test]
fn a_plant_tick_evaluates_its_roots_once_and_keeps_its_passes() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(TICKED_PLANT, "TickedPlant", false, &[0.95])
    {
        let counts = restricted.counts;
        assert_eq!(restricted.refusal, None, "{solver_mode:?}");
        // Nine ticks, each as the inventory has it: two outer passes, four
        // relation refreshes and one after the commit, three settle passes
        // and two condition-memory advances.
        assert_eq!(
            (
                counts.events,
                counts.outer_passes,
                counts.relation_refreshes,
                counts.post_commit_relation_refreshes,
                counts.discrete_passes,
                counts.condition_memory_passes,
            ),
            (9, 18, 36, 9, 27, 18),
            "{solver_mode:?}"
        );
        // The controller output feeds no root condition: of the five
        // refreshes per tick only the first evaluates.
        assert_eq!(counts.root_evaluations_skipped, 36, "{solver_mode:?}");
        assert!(counts.discrete_programs_skipped > 0, "{solver_mode:?}");
    }
}

/// Section 6, case 1: the clock body reaches nothing else.
const CLOCK_BODY_ONLY: &str = "model ClockBodyOnly \
    Real x(start=1, fixed=true); discrete Real s(start=0, fixed=true); Boolean high; \
    equation der(x) = -x; high = x > 0.5; \
    when sample(0, 0.1) then s = pre(s) + 1; end when; end ClockBodyOnly;";

#[test]
fn a_clock_body_that_reaches_nothing_else_skips_every_later_root_evaluation() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(CLOCK_BODY_ONLY, "ClockBodyOnly", false, &STOPS)
    {
        let counts = restricted.counts;
        assert_eq!(
            counts.root_evaluations_skipped,
            counts.relation_refreshes + counts.post_commit_relation_refreshes - counts.events,
            "{solver_mode:?}: one root evaluation per event"
        );
    }
}

/// Section 6, case 2: the clock body changes a variable a relation reads, the
/// relation memory flips and a `when` fires in the next pass.
const BODY_FLIPS_RELATION: &str = "model BodyFlipsRelation \
    Real x(start=0, fixed=true); discrete Real u(start=0, fixed=true); \
    discrete Integer k(start=0, fixed=true); \
    equation der(x) = 1; \
    when sample(0, 0.1) then u = if pre(u) > 0.5 then 0 else 1; end when; \
    when u > 0.5 then k = pre(k) + 1; end when; end BodyFlipsRelation;";

#[test]
fn a_clock_body_that_flips_a_relation_reevaluates_its_roots() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(BODY_FLIPS_RELATION, "BodyFlipsRelation", false, &STOPS)
    {
        let counts = restricted.counts;
        assert!(
            counts.root_evaluations > counts.events,
            "{solver_mode:?}: a dirty root runs again within its event: {counts:?}"
        );
        assert!(counts.root_evaluations_skipped > 0, "{solver_mode:?}");
        assert!(
            counts.outer_passes >= 2 * counts.events,
            "{solver_mode:?}: {counts:?}"
        );
    }
}

/// Section 6, case 3: a chain `a -> b -> c` over three passes, each link
/// reading the `pre` of the one before it as that `pre` advances.
const PRE_CHAIN: &str = "model PreChain \
    Real x(start=0, fixed=true); discrete Integer a(start=0, fixed=true); \
    discrete Integer b(start=0); discrete Integer c(start=0, fixed=true); \
    equation der(x) = 1; \
    when sample(0, 0.1) then a = pre(a) + 1; end when; \
    b = pre(a); \
    when change(b) then c = pre(c) + 1; end when; end PreChain;";

#[test]
fn a_chain_over_advancing_pre_reruns_each_link_in_the_pass_after_its_source_changed() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(PRE_CHAIN, "PreChain", false, &STOPS)
    {
        let counts = restricted.counts;
        assert!(
            counts.outer_passes >= 3 * counts.events,
            "{solver_mode:?}: {counts:?}"
        );
        assert!(
            counts.discrete_programs_skipped > 0,
            "{solver_mode:?}: {counts:?}"
        );
    }
}

/// Section 6, case 4: an unclocked `reinit` at a tick; slice 2 stays exact
/// through the state change it makes.
const REINIT_AT_TICK: &str = "model ReinitAtTick \
    Real x(start=1, fixed=true); discrete Real s(start=0, fixed=true); Boolean high; \
    equation der(x) = 1; high = x > 1.25; \
    when sample(0, 0.1) then s = pre(s) + 1; end when; \
    when s > 2.5 and s < 3.5 then reinit(x, 0); end when; end ReinitAtTick;";

#[test]
fn an_unclocked_reinit_at_a_tick_stays_exact() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(REINIT_AT_TICK, "ReinitAtTick", false, &STOPS)
    {
        assert_eq!(restricted.refusal, None, "{solver_mode:?}");
        assert!(
            restricted.counts.root_evaluations_skipped > 0,
            "{solver_mode:?}"
        );
    }
}

/// Section 6, case 5: the event changes the next event time through a
/// dynamic time event reading a changed discrete.
const MOVING_DEADLINE: &str = "model MovingDeadline \
    Real x(start=0, fixed=true); discrete Real deadline(start=0.05, fixed=true); \
    discrete Integer hits(start=0, fixed=true); \
    equation der(x) = 1; \
    when sample(0, 0.1) then deadline = time + 0.03; end when; \
    when time >= deadline then hits = pre(hits) + 1; end when; end MovingDeadline;";

#[test]
fn an_event_that_moves_the_next_event_time_stays_exact() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(MOVING_DEADLINE, "MovingDeadline", false, &STOPS)
    {
        let times = restricted
            .audits
            .iter()
            .map(|audit| audit.next_event_time)
            .collect::<std::collections::BTreeSet<_>>();
        assert!(times.len() > 2, "{solver_mode:?}: {times:?}");
        assert!(
            restricted.counts.root_evaluations_skipped
                + restricted.counts.discrete_programs_skipped
                > 0,
            "{solver_mode:?}"
        );
    }
}

/// Section 6, case 6: two clocks tick at the same instant.
const TWO_CLOCKS: &str = "model TwoClocksTick \
    Real x(start=0, fixed=true); discrete Real s(start=0, fixed=true); \
    discrete Real r(start=0, fixed=true); Boolean high; \
    equation der(x) = 1; high = x > 0.32; \
    when sample(0, 0.1) then s = pre(s) + 1; end when; \
    when sample(0, 0.05) then r = pre(r) + x; end when; end TwoClocksTick;";

#[test]
fn two_clocks_at_one_instant_stay_exact() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(TWO_CLOCKS, "TwoClocksTick", false, &STOPS)
    {
        assert!(
            restricted.counts.root_evaluations_skipped > 0,
            "{solver_mode:?}"
        );
    }
}

/// An assertion that fails inside an event update is returned as that
/// failure, from the state the full update leaves at that point.
const FAILING_ASSERT: &str = "model FailingAssert \
    Real x(start=0, fixed=true); discrete Real s(start=0, fixed=true); Boolean high; \
    equation der(x) = 1; high = x > 0.12; \
    when sample(0, 0.1) then s = pre(s) + 1; end when; \
    assert(s < 4, \"s reached four\"); end FailingAssert;";

#[test]
fn a_failure_inside_a_restricted_update_is_the_full_updates_failure() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(FAILING_ASSERT, "FailingAssert", false, &STOPS)
    {
        let failure = restricted.failure.as_deref().unwrap_or_default();
        assert!(
            failure.contains("s reached four"),
            "{solver_mode:?}: {failure}"
        );
        assert!(
            restricted.counts.root_evaluations_skipped > 0,
            "{solver_mode:?}"
        );
    }
}

#[test]
fn a_decoded_model_issues_its_own_reach_and_restricts_the_same_way() {
    let fresh = assert_restricted_matches_full(CLOCK_BODY_ONLY, "ClockBodyOnly", false, &STOPS);
    let decoded = assert_restricted_matches_full(CLOCK_BODY_ONLY, "ClockBodyOnly", true, &STOPS);
    for ((solver_mode, fresh), (_, decoded)) in fresh.iter().zip(&decoded) {
        assert_eq!(decoded.refusal, None, "{solver_mode:?}");
        assert_eq!(decoded.counts, fresh.counts, "{solver_mode:?}");
        assert_eq!(decoded.audits, fresh.audits, "{solver_mode:?}");
    }
}

/// A model with a delay channel: its event roots read history, so every
/// event update takes the full path, decided before it evaluates anything.
const DELAYED: &str = "model Delayed \
    Real x(start=1, fixed=true); Real d; discrete Real s(start=0, fixed=true); \
    equation der(x) = -x; d = delay(x, 0.05); \
    when sample(0, 0.1) then s = pre(s) + d; end when; end Delayed;";

#[test]
fn a_model_with_delays_takes_the_full_path() {
    for (solver_mode, restricted) in
        assert_restricted_matches_full(DELAYED, "Delayed", false, &STOPS)
    {
        assert_eq!(
            restricted.refusal,
            Some(EventReachRefusal::DelayChannels),
            "{solver_mode:?}"
        );
        assert_eq!(
            (
                restricted.counts.root_evaluations_skipped,
                restricted.counts.discrete_programs_skipped
            ),
            (0, 0),
            "{solver_mode:?}"
        );
    }
}
