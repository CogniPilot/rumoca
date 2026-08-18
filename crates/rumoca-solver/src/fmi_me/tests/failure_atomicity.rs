//! Failure atomicity of the common host's component excursions and of its
//! retained derivative capability (review findings [373], [374], [377], [378],
//! [381], [382]).
//!
//! Every ablation here injects a real component failure — the fixture's
//! algebraic branch has no real solution at a negative continuous state, so the
//! component's own checked numerics refuse the observation, the indicator
//! evaluation, or the derivative — or a real plugin failure through the public
//! checked contract. Nothing is stubbed and no test-only branch exists inside
//! the master algorithm.

use std::cell::Cell;

use rumoca_ir_solve as solve;

use super::{block, fixture_instance_config, nonlinear_right_limit_seed_model, refresh_owned};
use crate::fmi_me as me;

/// `der(x) = a` with the algebraic branch `a² = x`.
///
/// The branch has no real solution for `x < 0`, so every output observation and
/// every event-indicator evaluation the host performs at a negative continuous
/// state is a typed component failure raised by the component's own checked
/// numerics. That is the injected getter and indicator failure [373] asks for,
/// with no stubbed component and no test-only branch inside the host.
///
/// `with_indicator` decides which excursion the poisoned coordinate reaches
/// first: with an indicator the root scan hits it, without one the scan is
/// skipped over an empty inventory and the endpoint's event-left observation
/// hits it.
fn algebraic_branch_component(with_indicator: bool) -> solve::SolveModel {
    let mut model = nonlinear_right_limit_seed_model();
    if with_indicator {
        model.problem.events.root_conditions = block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "fmi_me_algebraic_branch_indicator.mo",
        );
        model.problem.events.root_zero_domains = vec![solve::RootZeroDomain::Positive];
    }
    refresh_owned(model)
}

/// The same component with one declared input, so an input write has a
/// correlated owner set to fall out of step ([382]).
fn branch_component_with_input() -> solve::SolveModel {
    let mut model = nonlinear_right_limit_seed_model();
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".to_string()];
    model.parameters = vec![1.0];
    refresh_owned(model)
}

/// The same component with one scheduled time event, so Event Mode's refresh
/// has a correlated owner set to fall out of step ([382]).
fn branch_component_with_event() -> solve::SolveModel {
    let mut model = nonlinear_right_limit_seed_model();
    model.problem.events.scheduled_time_events = vec![0.05];
    refresh_owned(model)
}

/// What a deliberately faulty plugin does to the host.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PluginFault {
    /// Behave correctly, so a successful path can prove every correlated owner
    /// becomes visible together.
    Healthy,
    /// Return a state the component cannot evaluate at every coordinate
    /// strictly inside the accepted interval, while staying exact at both
    /// endpoints.
    ///
    /// SPEC_0044 §6 says endpoint agreement is what the host checks and
    /// convergence evidence is what validates the interior, so this is the
    /// exact gap an off-point failure has to survive.
    PoisonsTheInterior,
    /// Evaluate derivatives at a trial coordinate the session never adopted and
    /// then fail the advance, which is [374]'s "a backend error must not strand
    /// a trial component point".
    FailsAfterATrialEvaluation,
    /// Provoke a component derivative failure, use every plugin-visible method
    /// on the retained handle to try to erase or replace it, and then report
    /// success ([377], [378], [381]).
    SuppressesTheComponentFailure,
    /// Ask the component for a derivative from inside `sample`, where the
    /// capability is inactive, catch and ignore the opaque refusal, and return
    /// `Ok`. The next host interaction must still surface the misuse rather
    /// than clear it ([381]).
    HidesAnInactiveRequest,
    /// Fail every `initialize` after the first, so a reset, an Event Mode
    /// refresh, and an input write each lose their plugin history after the
    /// component has already moved ([382]).
    FailsEveryInitializeAfterTheFirst,
    /// Evaluate derivatives at a trial coordinate the session never adopted and
    /// then **panic** out of the host call, which is [388]'s "unwinding must not
    /// bypass restoration and the usability record".
    PanicsAfterATrialEvaluation,
    /// Panic out of `sample`, strictly inside the accepted interval so both
    /// sampler wrappers can be reached: the scan's when the component declares
    /// an indicator, the session's event-left capture when it does not ([388]).
    ///
    /// `after_interior_samples` is how many interior samples the plugin serves
    /// correctly first. One is what makes the scan variant hostile rather than
    /// vacuous: the scan evaluates its indicators at each sampled coordinate, so
    /// surviving one coordinate leaves the component standing off the accepted
    /// point when the next sample unwinds ([390]).
    PanicsInsideTheSampler { after_interior_samples: usize },
    /// Ignore the request's coordinates entirely and report an endpoint far past
    /// every bound it carries ([398], [399]).
    ///
    /// Before the correlated shape this was the forgery: a plugin built its own
    /// `MeAdvanceRequest` around the endpoint it wanted and returned a proposal
    /// against *that*, so the host validated the step against the bound the
    /// plugin had chosen. There is now no request to forge, and the endpoint is
    /// checked against the bound the session is actually serving.
    IgnoresTheRequestAndCrossesTheBound,
    /// Return, verbatim, the endpoint an *earlier* call produced ([399]).
    ///
    /// The replay is a real one: the plugin caches the first accepted endpoint
    /// and hands the same numbers back on the second request, when the session
    /// already stands on that coordinate.
    ReplaysThePreviousCandidate,
    /// Report a candidate whose state vector is not the component's width
    /// ([399]).
    ///
    /// The arity comes from the linked component, so a plugin asserting its own
    /// cannot widen or narrow the checked point.
    ClaimsTheWrongStateArity,
    /// Report a legal candidate whose native continuous extension does not agree
    /// with it at the accepted endpoint: the sampler stays frozen at the
    /// interval's left value.
    ///
    /// SPEC_0044 §6 makes complete-interval sampling part of the accepted-step
    /// construction contract, so this must fail before the step is a proof.
    ContradictsItsOwnSamplerEndpoint,
}

/// The exact payload a hostile plugin panics with.
///
/// A dedicated type rather than a string, so an ablation can prove the host
/// resumed the **original** payload rather than a rendering of it: a
/// stringifying host could not produce this back.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HostilePanic {
    /// Panicked out of `advance`. `evaluated` records whether the trial
    /// derivative actually reached the component, so an ablation can prove the
    /// unwind really did start from a coordinate the session never adopted
    /// rather than from a plugin that never moved anything.
    AfterATrial { evaluated: bool },
    /// Panicked out of `sample`, where the capability is closed.
    /// `completed_interior_samples` records how many interior coordinates the
    /// sampler had already served, so an ablation can prove the scan really did
    /// evaluate indicators at one of them, and therefore really did move the
    /// component off the accepted point, before the unwind started ([390]).
    InTheSampler { completed_interior_samples: usize },
}

/// The original payload, or a failure of [388]'s no-stringification rule.
fn resumed_payload(caught: Box<dyn std::any::Any + Send>) -> HostilePanic {
    let Some(hostile) = caught.downcast_ref::<HostilePanic>() else {
        panic!("the host resumed something other than the plugin's own payload");
    };
    *hostile
}

/// A minimal explicit plugin built only from the public checked contract.
struct FaultyPlugin {
    derivatives: Option<me::MeDerivativeHandle>,
    step: Option<(f64, f64, f64, f64)>,
    fault: PluginFault,
    initializations: usize,
    /// How many strictly interior coordinates `sample` has already served. The
    /// sampler is `&self`, exactly as the host's closed-capability wrappers call
    /// it, so the counter a hostile sampler needs is an ordinary `Cell`.
    interior_samples: Cell<usize>,
    /// The endpoint an earlier `advance` produced, kept so a replaying plugin
    /// can hand exactly those numbers back on a later request ([399]).
    replayed: Option<(f64, f64)>,
}

impl FaultyPlugin {
    fn new(fault: PluginFault) -> Self {
        Self {
            derivatives: None,
            step: None,
            fault,
            initializations: 0,
            interior_samples: Cell::new(0),
            replayed: None,
        }
    }

    fn handle(&self) -> Result<&me::MeDerivativeHandle, me::MeIntegrationError> {
        self.derivatives.as_ref().ok_or_else(|| {
            me::MeIntegrationError::numerical(
                "faulty",
                me::MeNumericalFailure::Construction,
                "the plugin was never issued a derivative handle",
            )
        })
    }

    /// Do everything the plugin-visible surface allows to hide a component
    /// failure the host must see ([377], [378]).
    fn suppress_a_component_failure(&self, time: f64) -> Result<(), me::MeIntegrationError> {
        let handle = self.handle()?;
        // 1. Provoke the component failure through the *fallible* entry only,
        //    then catch and drop its result, keeping no cause. [378]: that
        //    entry must have latched before returning, or this alone would
        //    already have suppressed the failure.
        drop(handle.derivatives(time, &[-1.0]));
        // 2. Walk every other plugin-visible method, at a state the component
        //    accepts, in the hope of clearing or overwriting the latch. None of
        //    them consumes it, and none of them latches anything of its own.
        let mut out = [0.0];
        for _ in 0..3 {
            let _ = handle.has_failed();
            let _ = handle.state_count();
            handle.derivatives_into(time, &[4.0], &mut out);
        }
        Ok(())
    }
}

impl me::MeIntegratorBackend for FaultyPlugin {
    fn initialize(
        &mut self,
        point: &me::MeContinuousPoint,
        derivatives: me::MeDerivativeHandle,
    ) -> Result<(), me::MeIntegrationError> {
        self.initializations = self.initializations.saturating_add(1);
        if self.fault == PluginFault::FailsEveryInitializeAfterTheFirst && self.initializations > 1
        {
            return Err(me::MeIntegrationError::numerical(
                "faulty",
                me::MeNumericalFailure::Reset,
                "the plugin could not rebuild its history",
            ));
        }
        self.derivatives = Some(derivatives);
        let value = point.states()[0];
        self.step = Some((point.time(), value, point.time(), value));
        Ok(())
    }

    fn advance(
        &mut self,
        request: &me::MeAdvanceRequest,
    ) -> Result<me::MeStepCandidate, me::MeIntegrationError> {
        let t0 = request.current().time();
        let y0 = request.current().states()[0];
        let t1 = request.latest_accepted_time();
        if self.fault == PluginFault::SuppressesTheComponentFailure {
            // The host must outrank this `Ok` with the failure the plugin
            // provoked and then tried to bury ([381]).
            self.suppress_a_component_failure(t0)?;
        }
        if self.fault == PluginFault::PanicsAfterATrialEvaluation {
            // A real evaluation at a coordinate the session never adopts, over
            // a state the algebraic branch admits. Over a live component it
            // succeeds and leaves the component standing on that trial point;
            // the outcome travels in the payload rather than short-circuiting,
            // so the panic happens either way.
            let evaluated = self
                .handle()?
                .derivatives(0.5 * (t0 + t1), &[y0 + 1.0])
                .is_ok();
            std::panic::panic_any(HostilePanic::AfterATrial { evaluated });
        }
        let slope = self.handle()?.derivatives(t0, &[y0])?[0];
        if self.fault == PluginFault::FailsAfterATrialEvaluation {
            // A trial the host never adopts, followed by a library failure.
            self.handle()?
                .derivatives(0.5 * (t0 + t1), &[y0 + 0.5 * (t1 - t0) * slope])?;
            return Err(me::MeIntegrationError::numerical(
                "faulty",
                me::MeNumericalFailure::AdvanceExhausted,
                "the step controller gave up after its trial evaluation",
            ));
        }
        let y1 = y0 + (t1 - t0) * slope;
        match self.fault {
            PluginFault::IgnoresTheRequestAndCrossesTheBound => {
                // Computed without consulting a single coordinate of the
                // request: the plugin simply names the endpoint it wants. There
                // is no request object it could attach to justify it.
                let crossed = t0 + 999.0;
                return Ok(me::MeStepCandidate::new(
                    crossed,
                    vec![y0 + 999.0 * slope],
                    1,
                ));
            }
            PluginFault::ClaimsTheWrongStateArity => {
                // A second state the one-state component does not have.
                return Ok(me::MeStepCandidate::new(t1, vec![y1, y1], 1));
            }
            PluginFault::ReplaysThePreviousCandidate => {
                if let Some((time, value)) = self.replayed {
                    return Ok(me::MeStepCandidate::new(time, vec![value], 1));
                }
                self.replayed = Some((t1, y1));
            }
            _ => {}
        }
        self.step = Some((t0, y0, t1, y1));
        Ok(me::MeStepCandidate::new(t1, vec![y1], 1))
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), me::MeIntegrationError> {
        let Some((t0, y0, t1, y1)) = self.step else {
            return Err(me::MeIntegrationError::contract(
                "no accepted interval has been established",
            ));
        };
        if !time.is_finite() || time < t0 || time > t1 {
            return Err(me::MeIntegrationError::contract(format!(
                "t={time} lies outside [{t0}, {t1}]"
            )));
        }
        if time > t0 && time < t1 {
            // Strictly interior, so the endpoint samples the proposal proof
            // takes still succeed and the panic lands in whichever wrapper the
            // component's indicator inventory routes the interior sample to.
            let completed = self.interior_samples.get();
            if let PluginFault::PanicsInsideTheSampler {
                after_interior_samples,
            } = self.fault
                && completed >= after_interior_samples
            {
                std::panic::panic_any(HostilePanic::InTheSampler {
                    completed_interior_samples: completed,
                });
            }
            self.interior_samples.set(completed.saturating_add(1));
        }
        if self.fault == PluginFault::HidesAnInactiveRequest {
            // Inactive here by construction; catch and ignore the refusal.
            let _ = self.handle().map(|handle| handle.derivatives(time, &[y0]));
        }
        states[0] = if time.to_bits() == t0.to_bits() {
            y0
        } else if time.to_bits() == t1.to_bits() {
            if self.fault == PluginFault::ContradictsItsOwnSamplerEndpoint {
                // The continuous extension disagrees with the endpoint the same
                // plugin just reported, by far more than any state-consistency
                // bound admits.
                y1 + 1.0
            } else {
                y1
            }
        } else if self.fault == PluginFault::PoisonsTheInterior {
            // No real algebraic branch exists here, so the component refuses
            // every observation and every indicator evaluation at this state.
            -1.0
        } else {
            let theta = (time - t0) / (t1 - t0);
            y0 + theta * (y1 - y0)
        };
        Ok(())
    }

    fn truncate_reset(
        &mut self,
        point: &me::MeContinuousPoint,
    ) -> Result<(), me::MeIntegrationError> {
        let value = point.states()[0];
        self.step = Some((point.time(), value, point.time(), value));
        Ok(())
    }
}

/// One session over the algebraic-branch component driven by a faulty plugin.
fn faulty_session(
    retained: &mut me::session::MeRetainedComponent,
    fault: PluginFault,
) -> me::session::MeSimulationSession<'_, 'static> {
    use me::session::{MeSessionOptions, MeSessionOptionsInput};

    let options = MeSessionOptions::new(MeSessionOptionsInput {
        start_time: 0.0,
        stop_time: Some(1.0),
        relative_tolerance: 1.0e-8,
        absolute_tolerance: 1.0e-8,
        output_interval: 0.1,
        root_scan_resolution: 0.04,
        root_location_tolerance: 1.0e-10,
        max_wall_seconds: None,
        records_trace: true,
    })
    .expect("the ablation options are admissible");
    retained
        .lease(options)
        .expect("the sole lease is granted")
        .into_session(Some(Box::new(FaultyPlugin::new(fault))))
        .expect("a one-state component admits a one-state plugin")
}

fn retained_branch_component(model: &solve::SolveModel) -> me::session::MeRetainedComponent {
    me::session::MeRetainedComponent::instantiate(
        me::MeModelSource::fixture(model),
        &fixture_instance_config(),
        None,
    )
    .expect("the algebraic-branch component instantiates")
}

/// Ablation: an **output-getter** failure during the endpoint's event-left
/// observation.
///
/// Before [373] the host restored the accepted point only after `read_outputs`
/// succeeded, so this returned with `MeHostState.time/states` naming the
/// endpoint while the component still stood at the off-point coordinate.
#[test]
fn an_output_getter_failure_off_the_accepted_point_restores_that_point() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::PoisonsTheInterior);
    let mut cursor = me::session::MeOutputCursor::empty();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("the component cannot observe the poisoned event-left state");
    assert!(
        matches!(failure, me::session::MeSessionError::Component(_)),
        "the getter failure keeps its own category: {failure}"
    );
    assert_eq!(
        session.verification_session_point().0,
        0.1_f64.to_bits(),
        "the endpoint was accepted, so the failing excursion is the event-left observation \
         rather than anything in the scan"
    );

    assert_eq!(
        session.verification_component_point(),
        session.verification_session_point(),
        "the component must stand on exactly the session's accepted point"
    );
    // The failure happened after the plugin was asked to advance, so the
    // numerical history has no rollback proof and the session ends ([387]).
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: an **event-indicator** failure inside the root scan.
///
/// The scan deliberately walks the component across interior coordinates, so
/// this proves the excursion covers the whole scan rather than each success-path
/// caller.
#[test]
fn an_event_indicator_failure_inside_the_scan_restores_the_accepted_point() {
    let model = algebraic_branch_component(true);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::PoisonsTheInterior);
    let mut cursor = me::session::MeOutputCursor::empty();
    let before = session.verification_session_point();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("the component cannot evaluate indicators at the poisoned scan state");
    assert!(
        matches!(failure, me::session::MeSessionError::Component(_)),
        "the indicator failure keeps its own category: {failure}"
    );

    assert_eq!(
        session.verification_session_point(),
        before,
        "a failed scan accepts nothing"
    );
    assert_eq!(
        session.verification_component_point(),
        before,
        "the scan's last visited coordinate must not outlive the failure"
    );
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: a **backend** failure after it evaluated derivatives at a trial
/// coordinate ([374]).
///
/// The retained capability must be deactivated and the component must be back
/// on the accepted point, so the error strands neither.
#[test]
fn a_backend_failure_strands_neither_a_trial_point_nor_an_active_capability() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::FailsAfterATrialEvaluation);
    let mut cursor = me::session::MeOutputCursor::empty();
    let before = session.verification_session_point();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("the plugin abandoned its step");
    assert!(
        matches!(
            failure,
            me::session::MeSessionError::Integration(me::MeIntegrationError::Numerical {
                category: me::MeNumericalFailure::AdvanceExhausted,
                ..
            })
        ),
        "the library failure keeps its typed category: {failure}"
    );

    assert!(
        !session.verification_capability_is_active(),
        "the activation window closes on the failing exit too"
    );
    assert_eq!(session.verification_session_point(), before);
    assert_eq!(
        session.verification_component_point(),
        before,
        "the trial coordinate the plugin evaluated at must not survive the failure"
    );
    // [387]: the plugin promises no rollback of its own history, so the
    // restored component coordinate is not enough to keep the session callable.
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: a hostile backend tries to suppress the component's typed failure
/// through the plugin-visible handle surface ([377], [378]).
///
/// It catches and drops the fallible refusal, probes every other public entry,
/// and returns a generic library failure. The host must still report the
/// component's own typed failure, and must still restore the accepted point.
#[test]
fn a_backend_cannot_suppress_the_component_failure_it_provoked() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::SuppressesTheComponentFailure);
    let mut cursor = me::session::MeOutputCursor::empty();
    let before = session.verification_session_point();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("the component refused the derivative the plugin asked for");
    assert!(
        matches!(failure, me::session::MeSessionError::Component(_)),
        "the host must report the component's typed failure, not the backend's \
         substitute: {failure}"
    );
    assert!(
        failure
            .to_string()
            .contains("algebraic projection did not converge"),
        "the surviving failure is the component's own: {failure}"
    );

    assert!(!session.verification_capability_is_active());
    assert_eq!(session.verification_session_point(), before);
    assert_eq!(
        session.verification_component_point(),
        before,
        "the refused evaluation's coordinate must not survive either"
    );
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: a backend makes an inactive request from inside `sample`, catches
/// and ignores the opaque refusal, and returns `Ok` ([381]).
///
/// The host must surface the typed misuse at its next interaction rather than
/// clear it and proceed.
#[test]
fn an_ignored_inactive_request_surfaces_at_the_next_host_interaction() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::HidesAnInactiveRequest);
    let mut cursor = me::session::MeOutputCursor::empty();
    let before = session.verification_session_point();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("the plugin's sampler reached for the component");
    assert!(
        matches!(
            failure,
            me::session::MeSessionError::Integration(
                me::MeIntegrationError::DerivativeCapabilityInactive { .. }
            )
        ),
        "an ignored misuse must not be cleared: {failure}"
    );
    assert_eq!(session.verification_session_point(), before);
    assert_eq!(session.verification_component_point(), before);
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: a backend **panics** after evaluating derivatives at a trial
/// coordinate the session never adopted ([388]).
///
/// RAII closes the activation window on an unwind, but nothing else on the exit
/// path runs, so before the fix the component was left standing on the trial
/// point while the host still named the accepted one and `usability` was still
/// `None` — an embedding that caught the panic held a session with split
/// authority and no record of it.
#[test]
fn a_backend_panic_after_a_trial_evaluation_restores_the_accepted_point() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::PanicsAfterATrialEvaluation);
    let mut cursor = me::session::MeOutputCursor::empty();
    let before = session.verification_session_point();

    let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        session.advance_to(0.1, &mut cursor)
    }))
    .expect_err("the plugin's panic must still reach the embedding");
    assert_eq!(
        resumed_payload(caught),
        HostilePanic::AfterATrial { evaluated: true },
        "the original payload propagates unchanged — neither stringified nor turned \
         into a library error — and it records that the component really was moved \
         to the trial coordinate before the unwind"
    );

    assert!(
        !session.verification_capability_is_active(),
        "the activation window closes on the unwinding exit too"
    );
    assert_eq!(session.verification_session_point(), before);
    assert_eq!(
        session.verification_component_point(),
        before,
        "the trial coordinate the plugin evaluated at must not survive the unwind, \
         bit for bit"
    );
    // The component came back, but the plugin's own history unwound with no
    // rollback proof, so the caught session is not callable ([387], [388]).
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: the same unwind over a component that also refuses the restoration.
///
/// The more specific coordinate loss wins, so the caught session reports
/// `AcceptedPoint` rather than the numerical step that contained it.
#[test]
fn a_backend_panic_that_also_loses_the_accepted_point_records_that_loss() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::PanicsAfterATrialEvaluation);
    let mut cursor = me::session::MeOutputCursor::empty();
    session.verification_terminate_component();

    let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        session.advance_to(0.1, &mut cursor)
    }))
    .expect_err("a failed restoration must not swallow the payload either");
    assert_eq!(
        resumed_payload(caught),
        HostilePanic::AfterATrial { evaluated: false },
        "a terminated component refuses the trial evaluation and then the \
         restoration, and the payload still arrives unchanged"
    );

    assert!(!session.verification_capability_is_active());
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::AcceptedPoint);
}

/// Ablation: the plugin's native continuous extension **panics** ([388], [390]).
///
/// `sample` runs with the capability closed, so the unwind itself cannot have
/// moved the component; whoever called the sampler may already have, and that
/// is what decides who owes the accepted point back. Both wrappers are driven:
///
/// * with no indicator inventory the scan is skipped entirely, so the endpoint's
///   event-left capture takes the first interior sample through the session's
///   own wrapper. The session already stands on the endpoint it adopted, so the
///   only loss is the plugin's interior history.
/// * with one indicator the scan target takes it, and the sampler deliberately
///   survives the first interior coordinate. The scan evaluates indicators
///   there, leaving the component on that coordinate, off the accepted point
///   the session still names, and only the next sample unwinds. A sampler-local
///   catch would resume straight past `scan_with_retained`, so the enclosing
///   scan transaction is what has to catch, restore, and only then resume.
///
/// Either way the original payload reaches the embedding unchanged, the two
/// owners name one point bit for bit, and the caught session refuses every later
/// evaluation because the plugin promises no rollback of its own history.
#[test]
fn a_sampler_panic_restores_the_accepted_point_and_ends_the_session() {
    for (with_indicator, after_interior_samples) in [(false, 0_usize), (true, 1_usize)] {
        let model = algebraic_branch_component(with_indicator);
        let mut retained = retained_branch_component(&model);
        let mut session = faulty_session(
            &mut retained,
            PluginFault::PanicsInsideTheSampler {
                after_interior_samples,
            },
        );
        let mut cursor = me::session::MeOutputCursor::empty();
        let before = session.verification_session_point();

        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            session.advance_to(0.1, &mut cursor)
        }))
        .expect_err("the sampler's panic must still reach the embedding");
        assert_eq!(
            resumed_payload(caught),
            HostilePanic::InTheSampler {
                completed_interior_samples: after_interior_samples,
            },
            "the original payload propagates unchanged, and it records that the scan \
             variant really did serve, and have indicators evaluated at, an interior \
             coordinate before the unwind: {with_indicator}"
        );

        assert!(
            !session.verification_capability_is_active(),
            "the sampler never runs inside an open window: {with_indicator}"
        );
        if with_indicator {
            assert_eq!(
                session.verification_session_point(),
                before,
                "a scan that unwound accepted nothing, so the accepted point owed back \
                 is still the one the step started from"
            );
        } else {
            assert_eq!(
                session.verification_session_point().0,
                0.1_f64.to_bits(),
                "an empty inventory skips the scan, so the endpoint was already adopted \
                 when the event-left capture unwound"
            );
        }
        assert_eq!(
            session.verification_component_point(),
            session.verification_session_point(),
            "the interior coordinate the scan left the component on must not survive \
             the unwind, bit for bit: {with_indicator}"
        );
        assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
    }
}

/// Ablation: a backend ignores the request entirely and reports an endpoint far
/// past every bound the session is actually serving ([398], [399]).
///
/// This is the closed form of the forgery. The plugin cannot build, retain, or
/// return a `MeAdvanceRequest`, so it cannot supply the bound its own endpoint
/// would satisfy; the host binds the raw candidate to the request it is holding
/// and the crossing is a typed refusal before anything is sampled, adopted, or
/// published.
#[test]
fn a_candidate_that_ignores_the_request_cannot_cross_the_actual_bound() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(
        &mut retained,
        PluginFault::IgnoresTheRequestAndCrossesTheBound,
    );
    let mut cursor = me::session::MeOutputCursor::empty();
    let before = session.verification_session_point();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("the candidate crosses the yield boundary the session is serving");
    assert!(
        matches!(
            failure,
            me::session::MeSessionError::Integration(me::MeIntegrationError::Contract { .. })
        ),
        "an over-bound candidate is a typed plugin contract failure: {failure}"
    );
    assert!(
        failure.to_string().contains("past the host bound"),
        "the refusal names the host's own bound: {failure}"
    );

    assert_eq!(
        session.verification_session_point(),
        before,
        "a refused candidate accepts nothing"
    );
    assert_eq!(
        session.verification_component_point(),
        before,
        "and moves the component nowhere"
    );
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: a backend hands back, verbatim, the endpoint an earlier call
/// produced ([399]).
///
/// The first request is served honestly and the session adopts `t=0.1`. On the
/// next request, which starts at exactly that coordinate, the plugin returns
/// the same numbers again. Because the binding takes its start point from the
/// *actual* request rather than from the candidate, the replay is a step that
/// makes no progress at all, and there is no earlier request or proposal it
/// could be validated against instead.
#[test]
fn a_replayed_candidate_is_refused_against_the_request_that_follows_it() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::ReplaysThePreviousCandidate);
    let mut cursor = me::session::MeOutputCursor::empty();

    session
        .advance_to(0.1, &mut cursor)
        .expect("the first request is served honestly");
    let accepted = session.verification_session_point();
    assert_eq!(
        accepted.0,
        0.1_f64.to_bits(),
        "the replayed endpoint is the one the session now stands on"
    );

    let failure = session
        .advance_to(0.2, &mut cursor)
        .expect_err("the same candidate cannot serve the next request");
    assert!(
        matches!(
            failure,
            me::session::MeSessionError::Integration(me::MeIntegrationError::Contract { .. })
        ),
        "a replay is a typed plugin contract failure: {failure}"
    );
    assert!(
        failure.to_string().contains("no progress beyond roundoff"),
        "the refusal is measured from the session's actual current coordinate: {failure}"
    );

    assert_eq!(session.verification_session_point(), accepted);
    assert_eq!(session.verification_component_point(), accepted);
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: a backend reports a candidate of the wrong continuous-state width
/// ([399]).
///
/// The candidate cannot assert an arity: the width is taken from the linked
/// component, so a two-state vector for a one-state component is refused before
/// it can become a checked point.
#[test]
fn a_candidate_cannot_assert_a_state_arity_the_component_does_not_have() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::ClaimsTheWrongStateArity);
    let mut cursor = me::session::MeOutputCursor::empty();
    let before = session.verification_session_point();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("a two-state candidate cannot describe a one-state component");
    assert!(
        matches!(
            failure,
            me::session::MeSessionError::Integration(me::MeIntegrationError::Contract { .. })
        ),
        "a wrong-arity candidate is a typed plugin contract failure: {failure}"
    );
    assert!(
        failure
            .to_string()
            .contains("reports 2 states for a component of width 1"),
        "the component's own width decides: {failure}"
    );

    assert_eq!(session.verification_session_point(), before);
    assert_eq!(session.verification_component_point(), before);
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Ablation: a backend's native continuous extension contradicts the endpoint
/// the same backend just reported.
///
/// Binding proves the candidate against the request; it cannot prove that the
/// sampler covers the interval. That obligation belongs to the accepted-step
/// construction contract, so the mismatch has to fail there: before any
/// indicator is evaluated, any point is adopted, or any row is published.
#[test]
fn a_sampler_that_contradicts_its_own_endpoint_never_becomes_an_accepted_step() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::ContradictsItsOwnSamplerEndpoint);
    let mut cursor = me::session::MeOutputCursor::empty();
    let before = session.verification_session_point();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("the sampler disagrees with the endpoint it reported");
    let me::session::MeSessionError::Contract { reason } = &failure else {
        panic!("an endpoint disagreement is the host's own contract failure: {failure}");
    };
    assert!(
        reason.contains("accepted-interval right endpoint"),
        "the refusal names the endpoint that disagreed: {reason}"
    );

    assert_eq!(
        session.verification_session_point(),
        before,
        "an unvalidated sampler accepts nothing"
    );
    assert_eq!(session.verification_component_point(), before);
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::NumericalStep);
}

/// Every public entry a split session must refuse ([382]).
fn assert_split_session_is_refused(
    session: &mut me::session::MeSimulationSession<'_, 'static>,
    expected: me::session::MeSessionLoss,
) {
    use me::session::{MeOutputCursor, MeSessionError};

    let mut cursor = MeOutputCursor::empty();
    let attempts = [
        session.output_values().err(),
        session.set_input("u", 3.0).err(),
        session.reset(0.0).err(),
        session.advance_to(0.5, &mut cursor).err(),
    ];
    for outcome in attempts {
        let Some(MeSessionError::SessionNotReusable { loss }) = outcome else {
            panic!("a split session must refuse every mutating or evaluating call: {outcome:?}");
        };
        assert_eq!(loss, expected);
    }
    // Metadata and the already durable trace stay readable.
    assert_eq!(session.output_names().len(), 2);
}

/// Ablation: a restart loses the plugin's history after the component and every
/// host cache have already been rebuilt ([382]§3).
#[test]
fn a_restart_that_loses_the_plugin_history_ends_the_session() {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(
        &mut retained,
        PluginFault::FailsEveryInitializeAfterTheFirst,
    );

    let failure = session
        .reset(0.0)
        .expect_err("the plugin cannot rebuild its history");
    assert!(
        matches!(
            failure,
            me::session::MeSessionError::Integration(me::MeIntegrationError::Numerical {
                category: me::MeNumericalFailure::Reset,
                ..
            })
        ),
        "the original typed failure is returned unchanged: {failure}"
    );
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::Restart);
}

/// Ablation: Event Mode's refresh loses the plugin's history after the
/// component has already settled the event ([382]§1).
#[test]
fn an_event_refresh_that_loses_the_plugin_history_ends_the_session() {
    let model = branch_component_with_event();
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(
        &mut retained,
        PluginFault::FailsEveryInitializeAfterTheFirst,
    );
    let mut cursor = me::session::MeOutputCursor::empty();

    let failure = session
        .advance_to(0.1, &mut cursor)
        .expect_err("the plugin cannot rebuild its history across the event boundary");
    assert!(
        matches!(
            failure,
            me::session::MeSessionError::Integration(me::MeIntegrationError::Numerical {
                category: me::MeNumericalFailure::Reset,
                ..
            })
        ),
        "the original typed failure is returned unchanged: {failure}"
    );
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::EventRefresh);
}

/// Ablation: an input write loses the plugin's history after the component has
/// already taken the new value ([382]§2).
#[test]
fn an_input_write_that_loses_the_plugin_history_ends_the_session() {
    let model = branch_component_with_input();
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(
        &mut retained,
        PluginFault::FailsEveryInitializeAfterTheFirst,
    );

    let failure = session
        .set_input("u", 2.0)
        .expect_err("the plugin cannot rebuild its history after the input");
    assert!(
        matches!(
            failure,
            me::session::MeSessionError::Integration(me::MeIntegrationError::Numerical {
                category: me::MeNumericalFailure::Reset,
                ..
            })
        ),
        "the original typed failure is returned unchanged: {failure}"
    );
    assert_split_session_is_refused(&mut session, me::session::MeSessionLoss::InputApplication);
}

/// The successful counterparts: every correlated owner becomes visible
/// together, and a benign rejection leaves an ordinary live session ([382]).
#[test]
fn a_successful_input_and_restart_keep_every_owner_correlated() {
    let model = branch_component_with_input();
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::Healthy);
    let mut cursor = me::session::MeOutputCursor::empty();

    session
        .advance_to(0.05, &mut cursor)
        .expect("a healthy plugin advances");
    session.set_input("u", 2.5).expect("the input is applied");
    let visible = session.visible_values().expect("the session stays live");
    assert!((visible["u"] - 2.5).abs() <= f64::EPSILON);
    assert_eq!(
        session.verification_component_point(),
        session.verification_session_point(),
        "the component and the host name one point after the input"
    );

    // Rejections that move nothing leave the session ordinary and live: the
    // unknown name, and the component's own atomic batch rejection of a
    // non-finite value ([386]).
    assert!(matches!(
        session.set_input("not-an-input", 1.0),
        Err(me::session::MeSessionError::Contract { .. })
    ));
    assert!(matches!(
        session.set_input("u", f64::NAN),
        Err(me::session::MeSessionError::Component(_))
    ));
    let unchanged = session
        .visible_values()
        .expect("a rejected write leaves the session live");
    assert!(
        (unchanged["u"] - 2.5).abs() <= f64::EPSILON,
        "the rejected value never reached the input cache"
    );
    assert_eq!(
        session.verification_component_point(),
        session.verification_session_point()
    );
    session
        .set_input("u", 3.5)
        .expect("a later valid input still succeeds");
    assert!(matches!(
        session.reset(9.0),
        Err(me::session::MeSessionError::Options { .. })
    ));

    session
        .reset(0.0)
        .expect("the restart rebuilds the lifecycle");
    assert_eq!(session.time().to_bits(), 0.0_f64.to_bits());
    assert_eq!(
        session.verification_component_point(),
        session.verification_session_point()
    );
    session
        .advance_to(0.05, &mut cursor)
        .expect("the restarted session is still usable");
}

/// Ablation: the excursion's observation **and** its restoration both fail.
///
/// Typed precedence applies — the lost point outranks the observation and
/// carries it as data — and the session becomes an explicit non-reusable failed
/// session rather than an apparently live one with split authority.
#[test]
fn losing_the_accepted_point_outranks_the_observation_and_ends_the_session() {
    use me::session::{MeOutputCursor, MeSessionError};

    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(&mut retained, PluginFault::PoisonsTheInterior);

    let failure = session.verification_observe_off_point_after_terminate(0.05, &[4.0]);
    let MeSessionError::AcceptedPointLost {
        attempted: Some(attempted),
        ..
    } = &failure
    else {
        panic!("a failed restoration takes precedence and keeps its cause: {failure}");
    };
    assert!(matches!(attempted.as_ref(), MeSessionError::Component(_)));

    // Every later public call is refused; none of them is served from split
    // authority and none of them re-establishes the correlation.
    let mut cursor = MeOutputCursor::empty();
    for outcome in [
        session.advance_to(0.5, &mut cursor).err(),
        session.reset(0.0).err(),
        session.output_values().err(),
    ] {
        assert!(
            matches!(outcome, Some(MeSessionError::SessionNotReusable { .. })),
            "a session that lost its accepted point is not reusable: {outcome:?}"
        );
    }
}
