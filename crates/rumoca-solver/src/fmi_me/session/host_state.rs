//! Everything the FMI 3 ME master algorithm owns except the numerical plugin.
//!
//! Splitting this off keeps [`super`]'s public incremental surface and the
//! accepted-step loop readable inside the SPEC_0021 file and function limits.
//! It is not a second owner: `MeHostState` is private to the session module and
//! every operation here is reached only from the one master algorithm.

mod component;

use std::cell::{Cell, RefCell};

use indexmap::IndexMap;

use super::{MeSessionError, MeSessionLoss, MeSessionOptions, try_copied, try_filled};
use crate::{
    fmi_me::{
        MeCompletedIntegratorStep, MeDiscreteStates, MeError, MeFmuState, MeTime, SolveMeKernel,
        integrator::{
            MeContinuousPoint, MeDerivativeController, accepted_step_roundoff, canonical_coordinate,
        },
        trace::{MeTraceRecorder, TraceObservationRole},
    },
    runtime::timeout::TimeoutBudget,
    solver::{SimResult, SimTermination, SimVariableMeta},
};
pub(super) use component::MeHostComponent;

/// How many discrete iterations at one coordinate the host tolerates before it
/// declares the event fixed point non-convergent.
pub(super) const EVENT_ITERATION_LIMIT: usize = 256;

/// What FMI initialization settled, before any host state exists.
///
/// Returning this instead of mutating a half-built session is what lets the
/// session be constructed with an already-valid root policy: there is no
/// intermediate in which a stateful session holds a placeholder
pub(super) struct InitializationOutcome {
    pub(super) status: InitializationStatus,
    /// The settled output row at `startTime`, read while the component was
    /// still in a state where getters are legal.
    pub(super) initial_values: Option<Vec<f64>>,
}

/// The two valid results of initialization.
///
/// An active result owns component-width states and nominals proven together.
/// A terminated result retains the last component-width state read in Event
/// Mode before the standard termination transition, but it is not active
/// numerical authority.
pub(super) enum InitializationStatus {
    Active {
        states: Vec<f64>,
        nominals: Vec<f64>,
        next_event_time: Option<f64>,
    },
    Terminated {
        states: Vec<f64>,
        termination: SimTermination,
    },
}

impl InitializationOutcome {
    fn active(
        states: Vec<f64>,
        nominals: Vec<f64>,
        next_event_time: Option<f64>,
        initial_values: Option<Vec<f64>>,
    ) -> Self {
        Self {
            status: InitializationStatus::Active {
                states,
                nominals,
                next_event_time,
            },
            initial_values,
        }
    }

    fn terminated(
        states: Vec<f64>,
        termination: SimTermination,
        initial_values: Option<Vec<f64>>,
    ) -> Self {
        Self {
            status: InitializationStatus::Terminated {
                states,
                termination,
            },
            initial_values,
        }
    }
}

/// Run the FMI initialization sequence exactly once.
///
/// `terminateSimulation` short-circuits: the component is never asked to enter
/// Continuous-Time Mode afterwards. Its settled continuous state is read while
/// still in Event Mode, before the importer performs the termination.
pub(super) fn run_fmi_initialization(
    kernel: &mut SolveMeKernel,
    start_time: f64,
    records_outputs: bool,
) -> Result<InitializationOutcome, MeSessionError> {
    kernel.enter_initialization_mode(start_time)?;
    kernel.exit_initialization_mode()?;
    let discrete = update_discrete_states_to_completion(kernel, start_time)?;

    // Event Mode is a legal state for the output getters, so the settled
    // initialization row is read here — before the session concludes anything
    // about termination.
    let initial_values = if records_outputs {
        Some(read_outputs(kernel)?)
    } else {
        None
    };

    if let Some(termination) = discrete.terminate_simulation {
        // `terminateSimulation` is a request to the importer, not a lifecycle
        // transition performed behind its back.  Complete that standard
        // transition before constructing a host that claims to be terminated,
        // so the component and host cannot name different lifecycle states.
        let state_domain = kernel.continuous_state_domain();
        let mut states = try_filled(state_domain.len(), 0.0, "terminal initialization states")?;
        kernel.get_continuous_states(&mut states)?;
        kernel.terminate()?;
        return Ok(InitializationOutcome::terminated(
            states,
            termination,
            initial_values,
        ));
    }

    kernel.enter_continuous_time_mode()?;
    let state_domain = kernel.continuous_state_domain();
    let mut states = try_filled(state_domain.len(), 0.0, "initial continuous states")?;
    kernel.get_continuous_states(&mut states)?;
    let mut nominals = try_filled(state_domain.len(), 0.0, "initial state nominals")?;
    kernel.get_nominals_of_continuous_states(&mut nominals)?;
    Ok(InitializationOutcome::active(
        states,
        nominals,
        discrete.next_event_time,
        initial_values,
    ))
}

/// Drive `fmi3UpdateDiscreteStates` to its fixed point.
///
/// `terminateSimulation` ends the iteration immediately, even when
/// `discreteStatesNeedUpdate` is also true. Both continuous-state change flags
/// accumulate across the whole iteration; a later pass that clears them must
/// not erase an earlier one.
pub(super) fn update_discrete_states_to_completion(
    kernel: &mut SolveMeKernel,
    time: f64,
) -> Result<MeDiscreteStates, MeSessionError> {
    let mut discrete = kernel.update_discrete_states()?;
    let mut iterations = 1usize;
    let mut values_changed = discrete.values_of_continuous_states_changed;
    let mut nominals_changed = discrete.nominals_of_continuous_states_changed;
    while discrete.terminate_simulation.is_none() && discrete.discrete_states_need_update {
        iterations = iterations.saturating_add(1);
        if iterations > EVENT_ITERATION_LIMIT {
            return Err(MeSessionError::EventIterationDiverged {
                time,
                limit: EVENT_ITERATION_LIMIT,
            });
        }
        discrete = kernel.update_discrete_states()?;
        values_changed |= discrete.values_of_continuous_states_changed;
        nominals_changed |= discrete.nominals_of_continuous_states_changed;
    }
    discrete.values_of_continuous_states_changed = values_changed;
    discrete.nominals_of_continuous_states_changed = nominals_changed;
    Ok(discrete)
}

fn read_outputs(kernel: &mut SolveMeKernel) -> Result<Vec<f64>, MeError> {
    let names = kernel.model_description().output_names;
    let references = names
        .iter()
        .map(|name| {
            kernel
                .value_reference(name)
                .ok_or_else(|| MeError::Contract {
                    reason: format!("checked FMI output {name:?} has no value reference"),
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut values = me_float_buffer(references.len(), "Float64 output values")?;
    kernel.get_float64(&references, &mut values)?;
    Ok(values)
}

fn me_float_buffer(len: usize, context: &'static str) -> Result<Vec<f64>, MeError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(len)
        .map_err(|_| MeError::Allocation {
            context,
            entries: len,
        })?;
    values.extend(std::iter::repeat_n(0.0, len));
    Ok(values)
}

/// Consecutive events observed at one canonical coordinate.
///
/// The coordinate and its count are one runtime state, so reset and
/// coordinate changes cannot update only half of the event-streak fact. The
/// private raw counter is an implementation detail of this algorithmic state;
/// it is not a continuous-state-domain width.
pub(super) struct EventStreak {
    coordinate: Option<f64>,
    count: usize,
}

impl EventStreak {
    pub(super) const fn empty() -> Self {
        Self {
            coordinate: None,
            count: 0,
        }
    }

    pub(super) fn reset(&mut self) {
        *self = Self::empty();
    }

    fn record(&mut self, event_time: f64) -> bool {
        if self.coordinate.map(f64::to_bits) != Some(event_time.to_bits()) {
            self.coordinate = Some(event_time);
            self.count = 0;
        }
        self.count = self.count.saturating_add(1);
        self.count > EVENT_ITERATION_LIMIT
    }
}

/// Everything the master algorithm owns except the numerical plugin.
pub(super) struct MeHostState {
    /// The component and its root-search domain/storage are one value; there
    /// is no fieldwise constructor that can cross-pair them.
    pub(super) component: MeHostComponent,
    pub(super) trace: MeTraceRecorder,
    pub(super) options: MeSessionOptions,
    pub(super) budget: TimeoutBudget,
    /// The admitted start coordinate of this run, derived from the pristine
    /// component snapshot on lease and replaced atomically on reset.
    pub(super) start_time: f64,
    pub(super) time: f64,
    pub(super) states: Vec<f64>,
    /// The closed interval the plugin's native continuous extension currently
    /// covers, when one exists.
    ///
    /// SPEC_0044 §6 requires every left-limit observation to be materialized
    /// *while* that interval still exists. Tracking it is what lets the host
    /// sample the left coordinate instead of relabelling the exact event
    /// observation. It is cleared by a truncate and by
    /// any plugin history restart, because both destroy the extension.
    pub(super) retained_interval: Option<(f64, f64)>,
    pub(super) next_event_time: Option<f64>,
    pub(super) termination: Option<SimTermination>,
    pub(super) terminated: bool,
    pub(super) input_names: Vec<String>,
    pub(super) inputs: IndexMap<String, f64>,
    pub(super) pristine: MeFmuState,
    pub(super) event_streak: EventStreak,
    /// Why the session stopped being usable, once that has happened.
    ///
    /// After it is set, some pair of correlated owners — the component and the
    /// accepted point, the event refresh, the input caches, the lifecycle —
    /// name different states and nothing here can re-establish that, so every
    /// later mutating or evaluating call is refused rather than served from
    /// split authority. It is a [`Cell`] because
    /// an excursion closes on the `&self` observation paths.
    pub(super) usability: Cell<Option<MeSessionLoss>>,
}

impl MeHostState {
    // -- metadata ----------------------------------------------------------

    pub(super) fn kernel(&self) -> &RefCell<SolveMeKernel> {
        self.component.kernel()
    }

    pub(super) fn derivatives(&self) -> &MeDerivativeController {
        self.component.derivatives()
    }

    pub(super) fn output_names(&self) -> Vec<String> {
        self.kernel()
            .borrow_mut()
            .model_description()
            .output_names
            .to_vec()
    }

    pub(super) fn output_meta(&self) -> Vec<SimVariableMeta> {
        self.kernel()
            .borrow()
            .model_description()
            .output_meta
            .to_vec()
    }

    pub(super) fn state_domain(&self) -> crate::fmi_me::MeContinuousStateDomain {
        self.component.state_domain()
    }

    // -- event ownership ---------------------------------------------------

    /// Enter Event Mode at the coordinate the session already stands on.
    ///
    /// The caller adopts `event_time` first, so the component is already there:
    /// the lifecycle transition never moves the component away from the point
    /// the host names.
    pub(super) fn run_event_mode(
        &mut self,
        event_time: f64,
    ) -> Result<MeDiscreteStates, MeSessionError> {
        debug_assert_eq!(
            self.time.to_bits(),
            canonical_coordinate(event_time).to_bits(),
            "Event Mode is entered at the coordinate the session already adopted"
        );
        let mut kernel = self.kernel().borrow_mut();
        kernel.enter_event_mode()?;
        update_discrete_states_to_completion(&mut kernel, event_time)
    }

    pub(super) fn record_event_streak(&mut self, event_time: f64) -> Result<(), MeSessionError> {
        if self.event_streak.record(event_time) {
            return Err(MeSessionError::EventIterationDiverged {
                time: event_time,
                limit: EVENT_ITERATION_LIMIT,
            });
        }
        Ok(())
    }

    /// Issue the standard FMI termination transition and only then publish the
    /// matching host state.
    ///
    /// `termination` is `None` for an ordinary defined-experiment end and
    /// `Some` only when the component requested early successful termination.
    /// This ordering prevents a failed component transition from leaving the
    /// host falsely advertising Terminated.
    pub(super) fn terminate_component(
        &mut self,
        termination: Option<SimTermination>,
    ) -> Result<(), MeSessionError> {
        self.kernel().borrow_mut().terminate()?;
        self.terminated = true;
        self.termination = termination;
        Ok(())
    }

    pub(super) fn terminate_at_current_point(
        &mut self,
        message: &str,
    ) -> Result<(), MeSessionError> {
        let termination = SimTermination {
            time: self.time,
            message: message.to_owned(),
        };
        self.terminate_component(Some(termination))
    }

    // -- trace ownership ---------------------------------------------------

    /// Publish the settled initialization row.
    ///
    /// The values were read while the component was still in a state where the
    /// getters are legal, so this uses the atomic slice-record operation rather
    /// than an infallible copy into the closure form.
    pub(super) fn record_initialization(
        &mut self,
        time: f64,
        values: &[f64],
    ) -> Result<(), MeSessionError> {
        if !self.options.records_trace() {
            return Ok(());
        }
        self.trace
            .record_slice(TraceObservationRole::Initialization, time, values)?;
        Ok(())
    }

    /// Whether the trace already carries a row at exactly this coordinate.
    ///
    /// The master algorithm uses this to avoid *generating* a candidate the
    /// SPEC_0050 matrix does not authorize, rather than relying on the recorder
    /// to invent a suppression for it.
    pub(super) fn already_published_at(&self, time: f64) -> bool {
        self.trace
            .last_time()
            .is_some_and(|last| last.to_bits() == canonical_coordinate(time).to_bits())
    }

    /// Admit `candidate` as the left-limit coordinate of `event_time`, when it
    /// lies inside the already-published domain.
    ///
    /// Coordinate assignment is one decision made here, before any row reaches
    /// the recorder. The caller supplies the coordinate it will *also* read the
    /// component at — the checked `MeRootApplication::left()` for a located
    /// root, the canonical predecessor of an exact hard stop — so the row
    /// coordinate and the observed point are the same point by construction
    /// The settled value takes the exact event instant,
    /// so the axis stays nondecreasing.
    ///
    /// At `startTime`, at any instant whose left coordinate would fall behind
    /// the last published row, and at a degenerate bracket whose left limit is
    /// the event instant itself, there is no left-limit coordinate to publish
    /// separately. The host then generates no candidate rather than
    /// manufacturing an out-of-domain or duplicate one: the row already
    /// standing at or behind that coordinate *is* the left evidence
    pub(super) fn event_left_coordinate(&self, candidate: f64, event_time: f64) -> Option<f64> {
        admissible_event_left_coordinate(
            candidate,
            event_time,
            self.trace.last_time(),
            self.start_time,
        )
    }

    /// Whether the accepted endpoint reaches the cached standard
    /// `nextEventTime`.
    ///
    /// The host therefore knows, *before* the completed-step callback, that this
    /// endpoint is an event, and can make its left evidence durable first
    /// The coincidence rule is the master loop's:
    /// `|delta| <= roundoff`.
    /// Return the component-owned event coordinate reached by `endpoint`.
    ///
    /// A plugin may report a hard-stop endpoint a few ulps to either side of
    /// the requested coordinate. The coincidence proof admits that endpoint,
    /// but Event Mode must still run at the exact `nextEventTime` so Modelica
    /// time relations see their semantic boundary rather than the plugin's
    /// nearby numerical coordinate.
    pub(super) fn reached_cached_event_time(&self, endpoint: f64) -> Option<f64> {
        reached_event_time(self.next_event_time, endpoint)
    }

    pub(super) fn record_settled(&mut self, event_time: f64) -> Result<(), MeSessionError> {
        self.record_from_component(TraceObservationRole::Settled, event_time)
    }

    /// Publish values the host already observed at `time`.
    ///
    /// The recorder still makes one atomic decision-and-commit; what this
    /// avoids is running the FMI getters after the component has moved on
    pub(super) fn record_observed(
        &mut self,
        role: TraceObservationRole,
        time: f64,
        values: &[f64],
    ) -> Result<(), MeSessionError> {
        if !self.options.records_trace() {
            return Ok(());
        }
        self.trace.record_slice(role, time, values)?;
        Ok(())
    }

    /// Read the component's outputs at a coordinate the session does not stand
    /// on, then put it back on the session's own accepted point.
    pub(super) fn observe_off_point(
        &self,
        time: f64,
        states: &[f64],
    ) -> Result<Vec<f64>, MeSessionError> {
        self.anchor().observe_at(time, states)
    }

    /// Record the component's current outputs under `role`, deciding whether
    /// the row is needed **before** running any FMI getter.
    pub(super) fn record_from_component(
        &mut self,
        role: TraceObservationRole,
        time: f64,
    ) -> Result<(), MeSessionError> {
        if !self.options.records_trace() {
            return Ok(());
        }
        let Self {
            trace, component, ..
        } = self;
        let kernel = component.kernel();
        trace.record_with::<MeSessionError, _>(role, time, || Ok(observe_current(kernel)?))?;
        Ok(())
    }

    /// Record an observation at a coordinate the session does not stand on,
    /// again deciding first.
    ///
    /// The component is set to exactly `(time, states)` before the getters run
    /// and put back on the session's own accepted point afterwards, so the row
    /// coordinate and the evaluated point never disagree.
    pub(super) fn record_off_point(
        &mut self,
        role: TraceObservationRole,
        time: f64,
        states: &[f64],
    ) -> Result<(), MeSessionError> {
        if !self.options.records_trace() {
            return Ok(());
        }
        let Self {
            trace,
            component,
            usability,
            time: accepted_time,
            states: accepted_states,
            ..
        } = self;
        let anchor = AcceptedAnchor {
            kernel: component.kernel(),
            usability,
            time: *accepted_time,
            states: accepted_states,
        };
        trace.record_with::<MeSessionError, _>(role, time, || anchor.observe_at(time, states))?;
        Ok(())
    }

    pub(super) fn finish_trace(self) -> SimResult {
        let termination = self.termination.clone();
        self.trace.finish(termination)
    }

    // -- component compositions --------------------------------------------

    pub(super) fn checked_point(&self) -> Result<MeContinuousPoint, MeSessionError> {
        let states = try_copied(&self.states, "checked point states")?;
        Ok(MeContinuousPoint::new(
            self.time,
            states,
            self.state_domain(),
        )?)
    }

    /// Move the session **and** the component onto `(time, states)` together.
    ///
    /// The buffer is reserved before anything is committed, and the component
    /// is moved before the session commits, so every exit leaves the two naming
    /// one point: a failed reservation leaves both on the previous point, and a
    /// failed component move is restored to the previous point under the typed
    /// precedence rule (ME-BUF-001).
    pub(super) fn adopt_point(&mut self, time: f64, states: &[f64]) -> Result<(), MeSessionError> {
        let adopted = try_copied(states, "adopted point states")?;
        self.adopt_owned(time, Some(adopted))
    }

    /// Move the session and the component onto `time`, keeping the accepted
    /// state vector.
    pub(super) fn adopt_time(&mut self, time: f64) -> Result<(), MeSessionError> {
        self.adopt_owned(time, None)
    }

    /// The one transition that changes the accepted point.
    ///
    /// `states` of `None` keeps the current vector, which is what a coordinate
    /// change at a settled point needs.
    pub(super) fn adopt_owned(
        &mut self,
        time: f64,
        states: Option<Vec<f64>>,
    ) -> Result<(), MeSessionError> {
        // One accepted coordinate, canonically: `-0.0` and `0.0` are one FMI
        // time, and every same-coordinate rule the host applies is bitwise.
        let time = canonical_coordinate(time);
        let moved = {
            let target = states.as_deref().unwrap_or(&self.states);
            let mut kernel = self.kernel().borrow_mut();
            kernel
                .set_time(MeTime::at(time))
                .and_then(|()| kernel.set_continuous_states(target))
        };
        if let Err(error) = moved {
            // The component may hold the new time and the old states. It is put
            // back on the point the session still names, so the failure never
            // publishes a mixed coordinate.
            return self.anchor().settle(Err(MeSessionError::from(error)));
        }
        self.time = time;
        if let Some(states) = states {
            self.states = states;
        }
        Ok(())
    }

    pub(super) fn observe_current(&self) -> Result<Vec<f64>, MeError> {
        observe_current(self.kernel())
    }

    /// The accepted point every off-point excursion returns the component to.
    pub(super) fn anchor(&self) -> AcceptedAnchor<'_> {
        AcceptedAnchor {
            kernel: self.kernel(),
            usability: &self.usability,
            time: self.time,
            states: &self.states,
        }
    }

    /// Close an excursion whose body already ran, restoring the accepted point
    /// on success and on failure alike.
    pub(super) fn settle_after_excursion<T>(
        &self,
        attempted: Result<T, MeSessionError>,
    ) -> Result<T, MeSessionError> {
        self.anchor().settle(attempted)
    }

    /// Run one whole off-point excursion and close it on **every** exit.
    ///
    /// The body is free to walk the component away from the accepted point
    /// (that is what a proposal proof, a root scan, and its refinement do), so
    /// the transaction that owes the point back is also the one that has to
    /// survive an unwind out of the middle of it. A panic raised anywhere
    /// inside, including one an inactive sampler deliberately left to its
    /// caller, is caught here, the excursion is closed through the same
    /// [`AcceptedAnchor`] the returning exit uses, and only then does the
    /// **original** payload resume unwinding.
    pub(super) fn settle_caught_excursion<T>(
        &self,
        body: impl FnOnce() -> Result<T, MeSessionError>,
    ) -> Result<T, MeSessionError> {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(body)) {
            Ok(attempted) => self.settle_after_excursion(attempted),
            Err(payload) => {
                self.settle_unwound_excursion();
                std::panic::resume_unwind(payload);
            }
        }
    }

    /// Close an excursion whose body unwound instead of returning.
    ///
    /// A panic crossing a host wrapper skips every `?` the exit path is written
    /// on, so the two obligations that exit owes are discharged here, before the
    /// payload continues unwinding: the accepted point is restored through the
    /// same [`AcceptedAnchor`] the returning exit uses, and the typed loss is
    /// committed by [`unwound_loss`] — one write, deciding the accepted-point
    /// outcome *before* anything is recorded, so a plugin-history loss the same
    /// unwind already recorded downstack cannot hide a failed restoration
    pub(super) fn settle_unwound_excursion(&self) {
        let restored = self.anchor().restore().is_ok();
        self.usability
            .set(Some(unwound_loss(self.usability.get(), restored)));
    }

    /// Why the session stopped being usable, if it has.
    #[must_use]
    pub(super) fn usability_loss(&self) -> Option<MeSessionLoss> {
        self.usability.get()
    }

    /// Record why the session stopped being usable, first reason winning.
    ///
    /// The first loss is the cause; a later one is its consequence, and the
    /// more specific coordinate loss must not be overwritten by the mutating
    /// step that contained it.
    pub(super) fn mark_unusable(&self, loss: MeSessionLoss) {
        record_usability_loss(&self.usability, loss);
    }

    /// Close one mutating operation.
    ///
    /// A mutating step that fails after any correlated owner already moved
    /// cannot be left callable: the component, the host caches, the root
    /// policy, and the plugin's history would name different states, and the
    /// session would go on answering from whichever the caller happened to
    /// reach. The typed failure is returned unchanged and the session records
    /// only *why* it stopped being usable.
    pub(super) fn guard_mutation<T>(
        &self,
        loss: MeSessionLoss,
        outcome: Result<T, MeSessionError>,
    ) -> Result<T, MeSessionError> {
        if outcome.is_err() {
            self.mark_unusable(loss);
        }
        outcome
    }

    pub(super) fn read_nominals(&self) -> Result<Vec<f64>, MeSessionError> {
        let mut nominals = try_filled(self.state_domain().len(), 0.0, "state nominals")?;
        self.kernel()
            .borrow_mut()
            .get_nominals_of_continuous_states(&mut nominals)?;
        Ok(nominals)
    }

    pub(super) fn completed_integrator_step(
        &self,
    ) -> Result<Option<MeCompletedIntegratorStep>, MeError> {
        let mut kernel = self.kernel().borrow_mut();
        if !kernel.model_description().needs_completed_integrator_step {
            return Ok(None);
        }
        kernel.completed_integrator_step(true).map(Some)
    }

    /// Re-read the component's current maximum accepted-step duration.
    ///
    /// The value is never cached across an accepted step, a completed Event
    /// Mode, an input mutation, or a reset: it is read immediately before every
    /// one-step backend request.
    pub(super) fn read_max_step_duration(&self) -> Result<Option<f64>, MeError> {
        let Some(reference) = self.component.max_step_duration_reference() else {
            return Ok(None);
        };
        let mut values = [0.0];
        self.kernel()
            .borrow_mut()
            .get_float64(std::slice::from_ref(reference), &mut values)?;
        let limit = values[0];
        if limit == f64::MAX {
            // The documented maximum-finite sentinel means no bound currently
            // constrains the step.
            return Ok(None);
        }
        if !limit.is_finite() || limit <= 0.0 {
            return Err(MeError::Contract {
                reason: format!(
                    "the component reported a maximum step duration of {limit}, which is not \
                     finite and positive"
                ),
            });
        }
        Ok(Some(limit))
    }
}

/// Record why the session stopped being usable, first reason winning.
///
/// Shared with the borrow-split scan view, which holds the ledger without the
/// rest of the host state.
pub(super) fn record_usability_loss(usability: &Cell<Option<MeSessionLoss>>, loss: MeSessionLoss) {
    if usability.get().is_none() {
        usability.set(Some(loss));
    }
}

/// The typed precedence rule for closing one off-point excursion.
///
/// Kept free of the component so the decision itself can be exhausted rather
/// than reached only through a component that refuses its own accepted point.
///
/// A failed restoration outranks whatever the excursion was attempting: after
/// it the session and the component no longer name one point, so no later
/// observation from this session can be trusted, whereas a failed observation
/// over a restored point is an ordinary typed failure of a still-correlated
/// session. The attempted failure is neither discarded nor rendered into prose;
/// it travels inside [`MeSessionError::AcceptedPointLost`] as typed data
fn close_excursion<T>(
    accepted_time: f64,
    restoration: Result<(), MeError>,
    attempted: Result<T, MeSessionError>,
) -> Result<T, MeSessionError> {
    match restoration {
        Ok(()) => attempted,
        Err(restoration) => Err(MeSessionError::AcceptedPointLost {
            time: accepted_time,
            restoration: Box::new(restoration),
            attempted: attempted.err().map(Box::new),
        }),
    }
}

/// The typed loss an unwinding exit commits, decided before anything is
/// written.
///
/// Kept free of the component, like [`close_excursion`], so the one place where
/// a loss already in the ledger can be superseded is exhaustible on its own.
///
/// It is the same precedence rule, applied to the ledger instead of to the
/// returned value: a restoration that failed means the component and the
/// session no longer name one point, which outranks the numerical-history loss
/// the same unwind's sampler may already have recorded downstack. Every other
/// recorded loss belongs to an earlier, different transaction and is the cause
/// rather than the consequence, so it stays exactly where it is
fn unwound_loss(recorded: Option<MeSessionLoss>, restored: bool) -> MeSessionLoss {
    match (recorded, restored) {
        (Some(existing), true) => existing,
        (None, true) => MeSessionLoss::NumericalStep,
        (None | Some(MeSessionLoss::NumericalStep), false) => MeSessionLoss::AcceptedPoint,
        (Some(existing), false) => existing,
    }
}

/// The pure half of [`MeHostState::reached_cached_event_time`].
///
/// Kept free of the host so the one decision that makes an endpoint's left
/// evidence durable *before* `fmi3CompletedIntegratorStep` can be exhausted
/// directly. It is deliberately the master loop's own coincidence rule, so an
/// endpoint the loop will treat as an event is exactly an endpoint this admits.
fn reached_event_time(next_event_time: Option<f64>, endpoint: f64) -> Option<f64> {
    let roundoff = accepted_step_roundoff(endpoint, 0.0);
    next_event_time.filter(|event| (event - endpoint).abs() <= roundoff)
}

/// The pure half of [`MeHostState::event_left_coordinate`].
///
/// Kept free of the host so the coordinate rule itself can be exhausted rather
/// than exercised through a component.
fn admissible_event_left_coordinate(
    candidate: f64,
    event_time: f64,
    last_published: Option<f64>,
    start_time: f64,
) -> Option<f64> {
    let left = canonical_coordinate(candidate);
    if !left.is_finite() || left >= canonical_coordinate(event_time) {
        return None;
    }
    match last_published {
        // Strictly after the last published row: a left limit never lands on a
        // coordinate that already carries evidence, so the only authorized
        // same-coordinate cases can arise.
        Some(last) => (left > last).then_some(left),
        None => (left >= start_time).then_some(left),
    }
}

fn observe_current(kernel: &RefCell<SolveMeKernel>) -> Result<Vec<f64>, MeError> {
    let mut kernel = kernel.borrow_mut();
    read_outputs(&mut kernel)
}

/// The exact accepted point an off-point excursion returns the component to.
///
/// SPEC_0044 §6 and ME-BUF-001 make the session's accepted time/state and the
/// component's coordinate one fact. Every host operation that moves the
/// component away from it — a soft or event-left observation, an indicator
/// evaluation, a whole root scan and its refinement, a plugin call that
/// evaluates derivatives at trial points — runs inside one of these and is
/// closed by [`Self::settle`] on **every** exit, success or failure
pub(super) struct AcceptedAnchor<'host> {
    kernel: &'host RefCell<SolveMeKernel>,
    usability: &'host Cell<Option<MeSessionLoss>>,
    time: f64,
    states: &'host [f64],
}

impl AcceptedAnchor<'_> {
    /// `fmi3SetTime` + `fmi3SetContinuousStates` back onto the accepted point.
    fn restore(&self) -> Result<(), MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(self.time))?;
        kernel.set_continuous_states(self.states)
    }

    /// Close an excursion under the typed precedence rule, and mark the session
    /// non-reusable when the accepted point could not be re-established.
    pub(super) fn settle<T>(
        &self,
        attempted: Result<T, MeSessionError>,
    ) -> Result<T, MeSessionError> {
        let closed = close_excursion(self.time, self.restore(), attempted);
        if matches!(closed, Err(MeSessionError::AcceptedPointLost { .. })) {
            record_usability_loss(self.usability, MeSessionLoss::AcceptedPoint);
        }
        closed
    }

    /// Batched output getters at a coordinate the session does not stand on.
    pub(super) fn observe_at(&self, time: f64, states: &[f64]) -> Result<Vec<f64>, MeSessionError> {
        let attempted = self.read_at(time, states, observe_current);
        self.settle(attempted)
    }

    /// Move the component to `(time, states)` and read something there.
    ///
    /// The move itself is inside the excursion, so a setter that fails after
    /// `fmi3SetTime` succeeded is restored exactly like a failed getter.
    fn read_at(
        &self,
        time: f64,
        states: &[f64],
        read: impl FnOnce(&RefCell<SolveMeKernel>) -> Result<Vec<f64>, MeError>,
    ) -> Result<Vec<f64>, MeSessionError> {
        let states = try_copied(states, "off-point observation states")?;
        {
            let mut kernel = self.kernel.borrow_mut();
            kernel.set_time(MeTime::at(time))?;
            kernel.set_continuous_states(&states)?;
        }
        Ok(read(self.kernel)?)
    }
}

#[cfg(test)]
mod tests {
    use super::{
        MeError, MeSessionError, MeSessionLoss, admissible_event_left_coordinate, close_excursion,
        reached_event_time, unwound_loss,
    };
    use crate::timeline::event_left_limit_time;

    fn component_failure(reason: &str) -> MeError {
        MeError::Contract {
            reason: reason.to_owned(),
        }
    }

    /// All four ways an excursion can close. A restored point keeps the
    /// excursion's own answer, whatever it was; a lost point outranks it and
    /// keeps it as typed data.
    #[test]
    fn the_excursion_precedence_rule_is_exhaustive_and_loses_nothing() {
        // Observation succeeded, restoration succeeded: the value survives and
        // the session stays correlated.
        assert_eq!(
            close_excursion(0.5, Ok(()), Ok(7_usize)).expect("a closed excursion"),
            7
        );

        // Observation failed, restoration succeeded: an ordinary typed failure
        // of a session that still names one point.
        let observed = close_excursion::<usize>(
            0.5,
            Ok(()),
            Err(MeSessionError::Component(component_failure("getter"))),
        )
        .expect_err("the observation failed");
        assert!(
            matches!(observed, MeSessionError::Component(_)),
            "{observed}"
        );

        // Observation succeeded, restoration failed: the value is dropped,
        // because it was read at a point the session can no longer stand on.
        let lost = close_excursion(0.5, Err(component_failure("setter")), Ok(7_usize))
            .expect_err("a lost accepted point is a failure even so");
        let MeSessionError::AcceptedPointLost {
            time,
            restoration,
            attempted,
        } = &lost
        else {
            panic!("a failed restoration takes precedence: {lost}");
        };
        assert!((*time - 0.5).abs() <= f64::EPSILON);
        assert!(matches!(restoration.kind(), MeError::Contract { .. }));
        assert!(
            attempted.is_none(),
            "nothing was attempted-and-failed to carry"
        );

        // Both failed: restoration wins, and the attempted failure is retained
        // as typed data rather than folded into prose.
        let both = close_excursion::<usize>(
            0.5,
            Err(component_failure("setter")),
            Err(MeSessionError::Timeout { seconds: 1.5 }),
        )
        .expect_err("both halves failed");
        let MeSessionError::AcceptedPointLost {
            attempted: Some(attempted),
            ..
        } = &both
        else {
            panic!("the attempted failure must survive as typed data: {both}");
        };
        assert!(matches!(attempted.as_ref(), MeSessionError::Timeout { .. }));
    }

    /// The ledger half of the same precedence rule, for the exit that unwinds.
    ///
    /// A restored point leaves the plugin's own history as the only casualty. A
    /// **failed** restoration is the one outcome that outranks that, so it must
    /// be decided before anything is written rather than losing to a
    /// numerical-step loss the same unwind already recorded downstack; a loss
    /// from an earlier, different transaction is the cause rather than the
    /// consequence and still wins.
    #[test]
    fn the_unwinding_exit_decides_the_accepted_point_before_the_history_loss() {
        assert_eq!(unwound_loss(None, true), MeSessionLoss::NumericalStep);
        assert_eq!(unwound_loss(None, false), MeSessionLoss::AcceptedPoint);
        assert_eq!(
            unwound_loss(Some(MeSessionLoss::NumericalStep), true),
            MeSessionLoss::NumericalStep
        );
        assert_eq!(
            unwound_loss(Some(MeSessionLoss::NumericalStep), false),
            MeSessionLoss::AcceptedPoint
        );
        assert_eq!(
            unwound_loss(Some(MeSessionLoss::EventRefresh), true),
            MeSessionLoss::EventRefresh
        );
        assert_eq!(
            unwound_loss(Some(MeSessionLoss::EventRefresh), false),
            MeSessionLoss::EventRefresh
        );
    }

    /// The host knows an endpoint is an event before the callback exactly when
    /// the endpoint reaches the cached `nextEventTime`, under the master loop's
    /// own coincidence rule.
    #[test]
    fn an_endpoint_reaching_the_cached_next_event_time_is_known_to_be_an_event() {
        assert!(reached_event_time(Some(0.5), 0.5).is_some());
        // The checked step normalizes a public endpoint within roundoff onto
        // the host-issued coordinate, so a residual drift still coincides.
        assert!(reached_event_time(Some(0.5), 0.5 + f64::EPSILON).is_some());
        assert!(reached_event_time(Some(0.5), 0.4).is_none());
        assert!(reached_event_time(Some(0.5), 0.6).is_none());
        // An endpoint that is only the experiment stop or a yield is not an
        // event, so its left candidate is retained only if the callback asks.
        assert!(reached_event_time(None, 0.5).is_none());
    }

    #[test]
    fn a_roundoff_near_endpoint_preserves_the_exact_cached_event_coordinate() {
        let event = 0.026000000000000002_f64;
        let endpoint = event.next_down();

        let reached = reached_event_time(Some(event), endpoint)
            .expect("the adjacent hard-stop endpoint should reach the event");

        assert_eq!(reached.to_bits(), event.to_bits());
        assert_ne!(reached.to_bits(), endpoint.to_bits());
    }

    /// The checked left point of a located root is the row coordinate, so the
    /// rule has to admit an arbitrary refined coordinate, not only the
    /// canonical predecessor of the event instant.
    #[test]
    fn a_refined_left_coordinate_is_admitted_on_its_own_merits() {
        assert_eq!(
            admissible_event_left_coordinate(0.25, 0.25 + 1.0e-9, Some(0.2), 0.0),
            Some(0.25)
        );
        assert_eq!(
            admissible_event_left_coordinate(event_left_limit_time(0.5), 0.5, Some(0.4), 0.0),
            Some(event_left_limit_time(0.5))
        );
    }

    /// A left limit is never published at or after the coordinate that will
    /// carry the settled value: that pairing would put two different points on
    /// one coordinate and label the later one as left evidence.
    #[test]
    fn a_candidate_at_or_after_the_event_instant_publishes_no_left_row() {
        assert_eq!(
            admissible_event_left_coordinate(0.5, 0.5, Some(0.4), 0.0),
            None
        );
        assert_eq!(
            admissible_event_left_coordinate(0.6, 0.5, Some(0.4), 0.0),
            None
        );
        // A degenerate bracket the refinement could not separate.
        assert_eq!(admissible_event_left_coordinate(0.5, 0.5, None, 0.0), None);
    }

    /// The row already standing at or behind the candidate is the left
    /// evidence; a second row there is not authorized.
    #[test]
    fn a_candidate_behind_published_evidence_or_the_experiment_start_is_declined() {
        assert_eq!(
            admissible_event_left_coordinate(0.4, 0.5, Some(0.4), 0.0),
            None
        );
        assert_eq!(
            admissible_event_left_coordinate(0.3, 0.5, Some(0.4), 0.0),
            None
        );
        assert_eq!(
            admissible_event_left_coordinate(event_left_limit_time(0.0), 0.0, None, 0.0),
            None
        );
        assert_eq!(
            admissible_event_left_coordinate(f64::NAN, 0.5, None, 0.0),
            None
        );
    }

    /// `-0.0` and `0.0` are one FMI coordinate on both sides of the comparison.
    #[test]
    fn the_candidate_and_the_event_instant_are_compared_canonically() {
        assert_eq!(
            admissible_event_left_coordinate(-0.0, 0.0, None, -1.0),
            None
        );
        assert_eq!(
            admissible_event_left_coordinate(-0.5, -0.0, None, -1.0),
            Some(-0.5)
        );
    }
}
