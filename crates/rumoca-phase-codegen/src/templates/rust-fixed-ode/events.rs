// ---------------------------------------------------------------------------
// Event runtime. Hand-written plain Rust, identical for every model.
//
// The model above this point supplies only facts: its tables (roots, relation
// memories, pre lanes, discrete targets, clocks, time events), its row
// functions (`eval_roots`, `discrete_values`, `guarded_values`,
// `runtime_values`, `post_commit_values`) and the event iteration schedule the
// compiler issued (`EVENT_PASS`, `RELATION_PASS`, `SETTLE_PASS`). Everything
// below is the algorithm:
//
//   Simulator::do_step(h)
//     loop until t + h:
//       1. clip the sub-step at the next time event (instant or clock tick)
//       2. take one fixed step (Euler, Heun or RK4)
//       3. compare the sign of every event indicator with the stored domain
//       4. a changed domain is a state event; `EventLocation` decides where:
//          at the step end, or on the step's cubic Hermite interpolant
//       5. at the event: Appendix B event iteration (`ev_update`), then
//          re-seed the indicator domains and integrate the rest of the step
//
// The event iteration is the scalar event profile the FMI 3 C component
// executes (`fmi3/scalar_events.jinja`), relation-surface mode search
// included; state-event location follows the same plan (`RootBracket`,
// tolerance window, sliding modes).
// ---------------------------------------------------------------------------

/// Integration method of one fixed step.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Method {
    /// First-order forward Euler.
    Euler,
    /// Second-order Heun (explicit trapezoid).
    Heun,
    /// Classical fourth-order Runge-Kutta.
    Rk4,
}

/// Where a state event detected inside a step is applied.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventLocation {
    /// At the end of the step that saw the crossing. Costs nothing; the event
    /// is late by up to one step.
    StepEnd,
    /// After at most `EventSettings::secant_trials` Illinois secant refinements on the step's
    /// interpolant. Bounded cost; the event lands past the crossing, at the
    /// near end of what remains of the bracket.
    Secant,
    /// Refined until the bracket is no wider than the roundoff of the step.
    Bracketed,
}

/// Why a step failed. The simulator keeps the state it had before the step.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SimError {
    /// A model function failed or produced a non-finite root or derivative.
    Model,
    /// The event iteration did not settle within `FIXED_POINT_CAP` passes, or
    /// cycles among more relations than the mode search covers.
    EventIteration,
    /// More than `EventSettings::max_events_per_step` state events inside one
    /// step (Zeno).
    TooManyEvents,
    /// `do_step` was given a negative or non-finite step.
    InvalidStep,
    /// `do_step` ran before `initialize`.
    NotInitialized,
    /// A clock ticks faster than the time resolution can separate.
    UnresolvedClockTick,
    /// The error-level assertion with this index in `ACTIONS` failed.
    Assertion(usize),
}

/// The level of an assertion (`assert(..., level = ...)`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ActionKind {
    /// A failure stops the run with `SimError::Assertion`.
    Error,
    /// Never stops the run, creates an event, or changes a step.
    Warning,
}

impl From<SolveError> for SimError {
    fn from(_: SolveError) -> Self {
        SimError::Model
    }
}

/// Host-tunable limits of the event search.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EventSettings {
    /// Refinement budget of `EventLocation::Secant`.
    pub secant_trials: usize,
    /// State events one `do_step` may apply before it reports `TooManyEvents`;
    /// time events do not count.
    pub max_events_per_step: u32,
}

impl Default for EventSettings {
    fn default() -> Self {
        Self { secant_trials: 4, max_events_per_step: 64 }
    }
}
/// `RootLocationPlan::STANDARD`: machine epsilons of one interval's roundoff,
/// the minmax slack of the bracket, and the refinement cap.
const LOCATION_EPSILONS: f64 = 100.0;
const MINMAX_SLACK: u32 = 4;
const REFINEMENT_CAP: usize = 128;
/// `RootLocationPlan::STANDARD` sliding modes: an indicator slides after
/// `SLIDING_STREAK` consecutive state events each within `SLIDING_GAP` of the
/// last (the plan's gap fraction of its fallback scan resolution).
const SLIDING_STREAK: u32 = 8;
const SLIDING_GAP: f64 = 1.0e-5 * 1.0e-3;

/// A slot a row writes: `y[index]` or `p[index]`.
#[derive(Clone, Copy, Debug)]
pub struct Slot {
    pub index: usize,
    pub in_y: bool,
}

/// A pre binding: `p[dest]` holds `pre` of `y[source]` or `p[source]`.
#[derive(Clone, Copy, Debug)]
pub struct PreLane {
    pub dest: usize,
    pub source: usize,
    pub in_y: bool,
}

/// A periodic clock: tick `k` is at
/// `(phase_numerator + k * period_numerator) / denominator`, and
/// `p[activation]` reads 1 while it ticks.
#[derive(Clone, Copy, Debug)]
pub struct Clock {
    pub period: f64,
    pub phase: f64,
    pub phase_numerator: f64,
    pub period_numerator: f64,
    pub denominator: f64,
    pub activation: usize,
}

/// One step of an event pass (`EventPassStep`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventStep {
    RelationOverrides,
    RuntimeAssignments,
    AlgebraicProjection,
    RelationRefresh,
    RelationSettle,
}

/// One step of a relation pass (`RelationPassStep`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelationStep {
    DiscreteSettle,
    SettledIfRelationsUnchanged,
    AlgebraicProjection,
    RuntimeAssignments,
}

/// One step of a discrete settle pass (`SettleStep`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SettleStep {
    DiscreteOwners,
    RuntimeAssignments,
    SettledIfUnchanged,
    AlgebraicProjection,
}

type RowFn = fn(Scalar, &State, &Parameters, &mut [f64]) -> Result<(), SolveError>;

// ---------------------------------------------------------------------------
// The simulator: what a host calls.
// ---------------------------------------------------------------------------

/// A model advancing by fixed communication steps, events included.
#[derive(Clone, Debug)]
pub struct Simulator {
    method: Method,
    location: EventLocation,
    settings: EventSettings,
    time: Scalar,
    y: State,
    p: Parameters,
    events: EventMemory,
    /// The host wrote `p` since the last step.
    p_written: bool,
    sliding: SlidingModes,
    initialized: bool,
}

impl Simulator {
    /// A simulator at `time` with states `y` and parameters `p` (after
    /// `init_params`). Call `initialize` once before the first step.
    pub fn new(method: Method, location: EventLocation, settings: EventSettings, time: Scalar, y: State, p: Parameters) -> Self {
        Self { method, location, settings, time, y, p, events: EventMemory::new(), p_written: false, sliding: SlidingModes::new(), initialized: false }
    }

    /// Run the initial event at the start time: `initial()` and `pre`
    /// seeding, the `when` clauses active there, and clock ticks at the start.
    pub fn initialize(&mut self) -> Result<(), SimError> {
        let (time, mut y, mut p, mut events) = (self.time, self.y, self.p, self.events);
        ev_initialize(&mut events, time, &mut y, &mut p)?;
        ev_update(&mut events, time, &mut y, &mut p)?;
        ev_continuous(&mut events, time, &mut y, &p)?;
        ev_observe(time, &mut y, &mut p)?;
        ev_check_assertions(time, &y, &p)?;
        self.commit(time, y, p, events);
        self.initialized = true;
        Ok(())
    }

    pub fn time(&self) -> Scalar {
        self.time
    }

    /// States, then algebraic slots. The algebraic slots are current after an
    /// event; call `refresh_algebraics` on a copy to read them elsewhere.
    pub fn y(&self) -> &State {
        &self.y
    }

    pub fn p(&self) -> &Parameters {
        &self.p
    }

    /// Write inputs or tunable parameters between steps. A write that moves a
    /// relation across its surface is an event at the start of the next step;
    /// static time-event instants and clock ticks stay where they were compiled.
    pub fn p_mut(&mut self) -> &mut Parameters {
        self.p_written = true;
        &mut self.p
    }

    /// Advance exactly to `time() + h`, `h >= 0`. Returns the number of events
    /// applied. On error the simulator keeps the state it had before the call.
    pub fn do_step(&mut self, h: Scalar) -> Result<u32, SimError> {
        if !h.is_finite() || h < 0.0 {
            return Err(SimError::InvalidStep);
        }
        if !self.initialized {
            return Err(SimError::NotInitialized);
        }
        let end = self.time + h;
        let (mut time, mut y, mut p, mut events) = (self.time, self.y, self.p, self.events);
        let mut sliding = self.sliding;
        let (mut count, mut state_events) = (0, 0);
        let budget = match self.location {
            EventLocation::StepEnd => 0,
            EventLocation::Secant => self.settings.secant_trials,
            EventLocation::Bracketed => REFINEMENT_CAP,
        };
        // A host write to `p` may have moved a relation across its surface.
        if self.p_written {
            let crossed = ev_completed(&mut events, time, &mut y, &p)?;
            if crossed || ev_inputs_changed(&events, &p) {
                count_event(self.settings.max_events_per_step, &mut count, &mut state_events, true)?;
                apply_event(&mut events, time, &mut y, &mut p)?;
            }
        }
        while time < end {
            let (target, time_event) = match next_time_event(time, end)? {
                Some(instant) => (instant.min(end), true),
                None => (end, false),
            };
            let step = take_step(self.method, time, target, time_event, &y, &p)?;
            sliding.begin_interval();
            let mut z1 = [0.0; INDICATOR_SLOTS];
            if budget > 0 && INDICATORS > 0 {
                let mut y1 = step.y1;
                ev_indicators(&events, target, &mut y1, &p, &mut z1)?;
            }
            // A change made only by sliding indicators applies at the step end.
            if budget > 0 && domain_changed(&events, &z1) && !sliding.only_sliding(&events, &z1) {
                let mut at = locate(&events, &step, &p, &z1, budget)?;
                // A located event that matches the step's time event
                // applies with it, at the instant.
                if time_event && ev_time_match(at, target) {
                    at = target;
                }
                if (target - at).abs() > interval_roundoff(time, target - time) {
                    // The event lies inside the step: apply it there, then
                    // integrate the rest of the step from it.
                    time = at;
                    y = step.interpolate(at);
                    if !ev_completed(&mut events, time, &mut y, &p)? {
                        return Err(SimError::Model);
                    }
                    sliding.record_event(time, &events.changed);
                    count_event(self.settings.max_events_per_step, &mut count, &mut state_events, true)?;
                    apply_event(&mut events, time, &mut y, &mut p)?;
                    ev_check_assertions(ev_now(time), &y, &p)?;
                    continue;
                }
            }
            time = target;
            y = step.y1;
            let state_event = ev_completed(&mut events, time, &mut y, &p)?;
            if state_event {
                sliding.record_event(time, &events.changed);
            }
            if state_event || time_event {
                count_event(self.settings.max_events_per_step, &mut count, &mut state_events, state_event)?;
                apply_event(&mut events, time, &mut y, &mut p)?;
            }
            ev_check_assertions(ev_now(time), &y, &p)?;
        }
        ev_observe(ev_now(end), &mut y, &mut p)?;
        self.commit(end, y, p, events);
        self.sliding = sliding;
        self.p_written = false;
        Ok(count)
    }

    fn commit(&mut self, time: Scalar, y: State, p: Parameters, events: EventMemory) {
        self.time = time;
        self.y = y;
        self.p = p;
        self.events = events;
    }
}

/// Count one applied event; only state events count toward the Zeno cap.
fn count_event(cap: u32, count: &mut u32, state_events: &mut u32, state: bool) -> Result<(), SimError> {
    *count += 1;
    *state_events += u32::from(state);
    if *state_events > cap {
        return Err(SimError::TooManyEvents);
    }
    Ok(())
}

/// The event at `time`: assertions hold at its entry (the left limit), then
/// the event iteration runs.
fn apply_event(events: &mut EventMemory, time: Scalar, y: &mut State, p: &mut Parameters) -> Result<(), SimError> {
    ev_check_assertions(time, y, p)?;
    ev_update(events, time, y, p)?;
    ev_continuous(events, ev_now(time), y, p)
}

// ---------------------------------------------------------------------------
// One fixed step and its continuous extension.
// ---------------------------------------------------------------------------

/// One accepted step from `t0` to `t1`, with the slopes of its cubic Hermite
/// interpolant: the first stage and the last stage (Euler: the chord).
struct Step {
    t0: Scalar,
    t1: Scalar,
    y0: State,
    y1: State,
    f0: Derivative,
    f1: Derivative,
}

impl Step {
    /// The interpolated states at `t`; algebraic slots are left from `y1`.
    fn interpolate(&self, t: Scalar) -> State {
        let h = self.t1 - self.t0;
        let s = (t - self.t0) / h;
        let (s2, s3) = (s * s, s * s * s);
        let h00 = 2.0 * s3 - 3.0 * s2 + 1.0;
        let h10 = s3 - 2.0 * s2 + s;
        let h01 = -2.0 * s3 + 3.0 * s2;
        let h11 = s3 - s2;
        let mut y = self.y1;
        for i in 0..DERIVATIVE_LEN {
            y[i] = h00 * self.y0[i] + h10 * h * self.f0[i] + h01 * self.y1[i] + h11 * h * self.f1[i];
        }
        y
    }
}

fn add_scaled(base: &State, rate: &Derivative, scale: Scalar) -> State {
    let mut out = *base;
    for i in 0..DERIVATIVE_LEN {
        out[i] += scale * rate[i];
    }
    out
}

fn rhs(time: Scalar, y: &State, p: &Parameters) -> Result<Derivative, SimError> {
    let mut out = [0.0; DERIVATIVE_LEN];
    derivative_rhs_into(time, y, p, &mut out)?;
    if out.iter().any(|rate| !rate.is_finite()) {
        return Err(SimError::Model);
    }
    Ok(out)
}

/// One step of `method` from `t0` to `t1`. A step that ends at a time event
/// evaluates its end stage at the event's left limit.
fn take_step(
    method: Method,
    t0: Scalar,
    t1: Scalar,
    ends_at_time_event: bool,
    y0: &State,
    p: &Parameters,
) -> Result<Step, SimError> {
    let h = t1 - t0;
    let stage_end = if ends_at_time_event { t1.next_down() } else { t1 };
    let k1 = rhs(ev_now(t0), y0, p)?;
    let (y1, f1) = match method {
        Method::Euler => (add_scaled(y0, &k1, h), k1),
        Method::Heun => {
            let k2 = rhs(stage_end, &add_scaled(y0, &k1, h), p)?;
            let mut y1 = *y0;
            for i in 0..DERIVATIVE_LEN {
                y1[i] += 0.5 * h * (k1[i] + k2[i]);
            }
            (y1, k2)
        }
        Method::Rk4 => {
            let k2 = rhs(t0 + 0.5 * h, &add_scaled(y0, &k1, 0.5 * h), p)?;
            let k3 = rhs(t0 + 0.5 * h, &add_scaled(y0, &k2, 0.5 * h), p)?;
            let k4 = rhs(stage_end, &add_scaled(y0, &k3, h), p)?;
            let mut y1 = *y0;
            for i in 0..DERIVATIVE_LEN {
                y1[i] += h * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]) / 6.0;
            }
            (y1, k4)
        }
    };
    Ok(Step { t0, t1, y0: *y0, y1, f0: k1, f1 })
}

// ---------------------------------------------------------------------------
// State-event location.
// ---------------------------------------------------------------------------

/// The roundoff of an interval starting at `start` and lasting `duration`
/// (`RootLocationPlan::interval_roundoff`).
fn interval_roundoff(start: Scalar, duration: Scalar) -> Scalar {
    (LOCATION_EPSILONS * f64::EPSILON * (start.abs() + duration.abs())).max(f64::MIN_POSITIVE)
}

/// Per indicator: whether `z` left the domain stored at the last accepted point.
fn domain_changes(events: &EventMemory, z: &[f64; INDICATOR_SLOTS]) -> [bool; INDICATOR_SLOTS] {
    let mut changed = [false; INDICATOR_SLOTS];
    for k in 0..INDICATORS {
        changed[k] = events.domain[k] != (z[k] > 0.0);
    }
    changed
}

fn domain_changed(events: &EventMemory, z: &[f64; INDICATOR_SLOTS]) -> bool {
    domain_changes(events, z).contains(&true)
}

/// The coordinate in `(t0, t1]` at which the event that changed `z1` is
/// applied: always on the far side of the earliest crossing, after at most
/// `budget` evaluations of the indicators on the step's interpolant.
fn locate(
    events: &EventMemory,
    step: &Step,
    p: &Parameters,
    z1: &[f64; INDICATOR_SLOTS],
    budget: usize,
) -> Result<Scalar, SimError> {
    let changed = domain_changes(events, z1);
    let entered = |z: &[f64; INDICATOR_SLOTS], k: usize| changed[k] && events.domain[k] != (z[k] > 0.0);
    let evaluate_at = |t: Scalar, now: Scalar| -> Result<[f64; INDICATOR_SLOTS], SimError> {
        let mut y = step.interpolate(t);
        let mut z = [0.0; INDICATOR_SLOTS];
        ev_indicators(events, now, &mut y, p, &mut z)?;
        Ok(z)
    };
    let evaluate = |t: Scalar| evaluate_at(t, t);
    let mut z_low = evaluate_at(step.t0, ev_now(step.t0))?;
    let mut z_high = *z1;
    let tolerance = interval_roundoff(step.t0, step.t1 - step.t0).min(step.t1 - step.t0);
    let mut bracket = RootBracket::open(step.t0, step.t1, tolerance, MINMAX_SLACK);
    for _ in 0..budget {
        if bracket.is_located() {
            break;
        }
        let mut least = 1.0_f64;
        for k in 0..INDICATORS {
            if entered(&z_high, k) {
                least = least.min(bracket.crossing_fraction(z_low[k], z_high[k]));
            }
        }
        let Some(trial) = bracket.trial(least) else {
            break;
        };
        let z = evaluate(trial)?;
        let hit = (0..INDICATORS).any(|k| entered(&z, k));
        bracket.narrow(trial, hit);
        if hit {
            z_high = z;
        } else {
            z_low = z;
        }
    }
    if !bracket.is_located() {
        return Ok(bracket.high());
    }
    // RootTieBreak::ToleranceWindow: every change within one tolerance of the
    // earliest applies with it, at the window end, unless none stands there.
    let window = bracket.application_window(step.t1);
    if window > bracket.high() {
        let z = evaluate(window)?;
        if (0..INDICATORS).any(|k| entered(&z, k)) {
            return Ok(window);
        }
    }
    Ok(bracket.high())
}

// ---------------------------------------------------------------------------
// Time events: static instants and periodic clock ticks.
// ---------------------------------------------------------------------------

fn ev_time_match(a: f64, b: f64) -> bool {
    (a - b).abs() <= TIME_MATCH_TOLERANCE * (1.0 + a.abs().max(b.abs()))
}

fn time_in_window(event: f64, current: f64, target: f64) -> bool {
    event.is_finite() && event > current && (event < target || ev_time_match(event, target))
}

fn clock_tick_time(clock: &Clock, tick: f64) -> f64 {
    (clock.phase_numerator + tick * clock.period_numerator) / clock.denominator
}

fn clock_active(clock: &Clock, t: f64) -> bool {
    if !clock.period.is_finite() || !clock.phase.is_finite() || clock.period <= 0.0 || t + clock.period < clock.phase {
        return false;
    }
    let ticks = (t - clock.phase) / clock.period;
    let tick = ticks.round();
    tick >= 0.0 && ev_time_match(ticks, tick)
}

/// The first tick of `clock` after `current` and within `target`.
fn clock_next(clock: &Clock, current: f64, target: f64) -> Result<Option<f64>, SimError> {
    if !clock.period.is_finite() || !clock.phase.is_finite() || clock.period <= 0.0 || !current.is_finite() || !target.is_finite() {
        return Ok(None);
    }
    let mut tick = ((current - clock.phase) / clock.period).floor().max(0.0);
    for _ in 0..4 {
        let at = clock_tick_time(clock, tick);
        if time_in_window(at, current, target) && !ev_time_match(at, current) {
            return Ok(Some(at));
        }
        if at > target && !ev_time_match(at, target) {
            return Ok(None);
        }
        let first_distinct = current + TIME_MATCH_TOLERANCE * (1.0 + at.abs().max(current.abs()));
        let next = (((first_distinct - clock.phase) / clock.period).floor() + 1.0).max(tick + 1.0).max(0.0);
        if !next.is_finite() || next <= tick {
            return Ok(None);
        }
        tick = next;
    }
    Err(SimError::UnresolvedClockTick)
}

/// The earliest time event after `current` and within `target`.
fn next_time_event(current: f64, target: f64) -> Result<Option<f64>, SimError> {
    let mut next: Option<f64> = None;
    for &at in TIME_EVENTS {
        if at > current && !ev_time_match(at, current) && (at < target || ev_time_match(at, target)) {
            next = Some(next.map_or(at, |n| n.min(at)));
        }
    }
    for clock in CLOCKS {
        if let Some(at) = clock_next(clock, current, target)? {
            next = Some(next.map_or(at, |n| n.min(at)));
        }
    }
    Ok(next)
}

fn write_clock_lanes(p: &mut Parameters, t: f64) {
    for clock in CLOCKS {
        p[clock.activation] = if clock_active(clock, t) { 1.0 } else { 0.0 };
    }
}

fn clear_clock_lanes(p: &mut Parameters) {
    for clock in CLOCKS {
        p[clock.activation] = 0.0;
    }
}

// ---------------------------------------------------------------------------
// Event iteration (MLS Appendix B), as the FMI 3 C component runs it.
// ---------------------------------------------------------------------------

/// Event bookkeeping kept between steps.
#[derive(Clone, Copy, Debug)]
struct EventMemory {
    /// Per indicator: whether it was positive at the last accepted point.
    domain: [bool; INDICATOR_SLOTS],
    domain_valid: bool,
    /// The coordinate at event entry: the left limit `pre` reads.
    entry_y: State,
    entry_p: Parameters,
    entry_captured: bool,
    initial_event: bool,
    overrides: Overrides,
    /// The current event pass: a clocked owner runs in the first one only.
    pass: usize,
    /// The indicators the latest state event changed.
    changed: [bool; INDICATOR_SLOTS],
    /// The discrete inputs as the latest event saw them.
    inputs_seen: [f64; INPUT_SLOTS],
}

impl EventMemory {
    const fn new() -> Self {
        Self {
            domain: [false; INDICATOR_SLOTS],
            domain_valid: false,
            entry_y: [0.0; Y_LEN],
            entry_p: [0.0; P_LEN],
            entry_captured: false,
            initial_event: false,
            overrides: Overrides::new(),
            pass: 0,
            changed: [false; INDICATOR_SLOTS],
            inputs_seen: [0.0; INPUT_SLOTS],
        }
    }

    /// Start a new event at this coordinate: the left limit `pre` reads.
    fn capture_entry(&mut self, y: &State, p: &Parameters) {
        self.entry_y = *y;
        self.entry_p = *p;
        self.entry_captured = true;
        self.overrides.clear();
    }
}

/// Roots whose located side overrides their relation memory.
#[derive(Clone, Copy, Debug)]
struct Overrides {
    root: [usize; ROOT_SLOTS],
    value: [f64; ROOT_SLOTS],
    count: usize,
}

impl Overrides {
    const fn new() -> Self {
        Self { root: [0; ROOT_SLOTS], value: [0.0; ROOT_SLOTS], count: 0 }
    }

    fn clear(&mut self) {
        self.count = 0;
    }

    fn push(&mut self, root: usize, value: f64) {
        self.root[self.count] = root;
        self.value[self.count] = value;
        self.count += 1;
    }

    fn iter(&self) -> impl Iterator<Item = (usize, f64)> + '_ {
        self.root[..self.count].iter().copied().zip(self.value[..self.count].iter().copied())
    }

    fn contains(&self, root: usize) -> bool {
        self.root[..self.count].contains(&root)
    }

    /// The value an overridden relation memory keeps, if `memory` is one.
    fn for_memory(&self, memory: usize) -> Option<f64> {
        self.iter().find(|&(root, _)| ROOT_MEMORY[root] == Some(memory)).map(|(_, value)| value)
    }

    /// Keep the overrides `keep` accepts, in order; whether any was dropped.
    fn retain(&mut self, mut keep: impl FnMut(usize, f64) -> bool) -> bool {
        let mut kept = 0;
        for k in 0..self.count {
            let (root, value) = (self.root[k], self.value[k]);
            if keep(root, value) {
                self.root[kept] = root;
                self.value[kept] = value;
                kept += 1;
            }
        }
        let dropped = kept != self.count;
        self.count = kept;
        dropped
    }
}

/// An exact zero root reads as its zero domain's side.
fn ev_orient_root(root: f64, zero: i8) -> f64 {
    if root != 0.0 {
        return root;
    }
    match zero {
        1 => f64::EPSILON,
        -1 => -f64::EPSILON,
        _ => root,
    }
}

/// The relation memory a root encodes: 1 on its negative side.
fn ev_memory_value(root: f64) -> f64 {
    if root < 0.0 { 1.0 } else { 0.0 }
}

fn ev_value_changed(before: f64, after: f64) -> bool {
    if before.to_bits() == after.to_bits() {
        return false;
    }
    !before.is_finite() || !after.is_finite() || (before - after).abs() > 0.0
}

/// Write `value` into `slot`; whether its bits changed.
fn ev_write(slot: &mut f64, value: f64) -> bool {
    let changed = slot.to_bits() != value.to_bits();
    *slot = value;
    changed
}

fn slot_mut<'a>(y: &'a mut State, p: &'a mut Parameters, slot: Slot) -> &'a mut f64 {
    if slot.in_y { &mut y[slot.index] } else { &mut p[slot.index] }
}

fn ev_roots(time: Scalar, y: &State, p: &Parameters) -> Result<[f64; ROOT_SLOTS], SimError> {
    let mut roots = [0.0; ROOT_SLOTS];
    eval_roots(time, y, p, &mut roots)?;
    if roots[..N_ROOTS].iter().any(|root| !root.is_finite()) {
        return Err(SimError::Model);
    }
    Ok(roots)
}

/// The searched roots at the refreshed coordinate, an exact zero reported on
/// its domain's side.
fn ev_indicators(
    events: &EventMemory,
    time: Scalar,
    y: &mut State,
    p: &Parameters,
    z: &mut [f64; INDICATOR_SLOTS],
) -> Result<(), SimError> {
    if INDICATORS == 0 {
        return Ok(());
    }
    refresh_algebraics(time, y, p)?;
    let roots = ev_roots(time, y, p)?;
    for k in 0..INDICATORS {
        let root = INDICATOR_ROOT[k];
        let mut value = ev_orient_root(roots[root], ROOT_ZERO[root]);
        if value == 0.0 {
            let frozen_positive = match ROOT_MEMORY[root] {
                Some(memory) => p[memory] <= 0.5,
                None => events.domain_valid && events.domain[k],
            };
            value = if frozen_positive { f64::EPSILON } else { -f64::EPSILON };
        }
        z[k] = value;
    }
    Ok(())
}

/// Compare the indicators at `time` with the stored domains. A change
/// captures the event entry and arms each crossed root's new side; returns
/// whether there was one.
fn ev_completed(events: &mut EventMemory, time: Scalar, y: &mut State, p: &Parameters) -> Result<bool, SimError> {
    if INDICATORS == 0 {
        return Ok(false);
    }
    let mut z = [0.0; INDICATOR_SLOTS];
    ev_indicators(events, time, y, p, &mut z)?;
    if !events.domain_valid {
        for k in 0..INDICATORS {
            events.domain[k] = z[k] > 0.0;
        }
        events.domain_valid = true;
        return Ok(false);
    }
    let changed = domain_changes(events, &z);
    if !changed.contains(&true) {
        return Ok(false);
    }
    events.capture_entry(y, p);
    events.changed = changed;
    for k in 0..INDICATORS {
        let now = z[k] > 0.0;
        if changed[k] {
            events.overrides.push(INDICATOR_ROOT[k], if now { 0.0 } else { 1.0 });
        }
        events.domain[k] = now;
    }
    Ok(true)
}

fn ev_apply_overrides(events: &EventMemory, p: &mut Parameters) -> bool {
    let mut changed = false;
    for (root, value) in events.overrides.iter() {
        if let Some(memory) = ROOT_MEMORY[root] {
            changed |= ev_write(&mut p[memory], value);
        }
    }
    changed
}

/// Release the overrides the roots now contradict, then encode every other
/// root into its relation memory (only the algebraic-dependent ones with
/// `algebraic_only`).
fn ev_relation_refresh(
    events: &mut EventMemory,
    time: Scalar,
    y: &State,
    p: &mut Parameters,
    algebraic_only: bool,
) -> Result<bool, SimError> {
    let roots = ev_roots(time, y, p)?;
    let mut changed = events.overrides.retain(|root, value| {
        let include = !algebraic_only || ROOT_ALGEBRAIC[root];
        !include || roots[root] == 0.0 || ev_memory_value(roots[root]) == value
    });
    for root in 0..N_ROOTS {
        let Some(memory) = ROOT_MEMORY[root] else {
            continue;
        };
        if (algebraic_only && !ROOT_ALGEBRAIC[root]) || events.overrides.contains(root) {
            continue;
        }
        changed |= ev_write(&mut p[memory], ev_memory_value(ev_orient_root(roots[root], ROOT_ZERO[root])));
    }
    Ok(changed)
}

/// Refresh the algebraic coordinate; whether it changed.
fn ev_projection(time: Scalar, y: &mut State, p: &Parameters) -> Result<bool, SimError> {
    let before = *y;
    refresh_algebraics(time, y, p)?;
    Ok(before.iter().zip(y.iter()).any(|(before, after)| ev_value_changed(*before, *after)))
}

/// A guarded write to a state: a `reinit`, applied by `ev_reinit`.
fn ev_is_reinit(slot: Slot) -> bool {
    slot.in_y && slot.index < DERIVATIVE_LEN
}

/// Whether guarded owner `g` runs: a clocked one on its tick in the first
/// pass only.
fn ev_guard_runs(events: &EventMemory, g: usize, time: Scalar) -> bool {
    match GUARDED_CLOCK[g] {
        Some(clock) => events.pass == 0 && clock_active(&CLOCKS[clock], time),
        None => true,
    }
}

/// Evaluate the discrete owners on one snapshot, then write: the discrete
/// equations (or, with `memories_only`, the condition memories) and the
/// guarded assignments other than `reinit`. An overridden relation memory
/// keeps its located side.
fn ev_discrete_owners(
    events: &EventMemory,
    time: Scalar,
    y: &mut State,
    p: &mut Parameters,
    memories_only: bool,
) -> Result<bool, SimError> {
    let mut changed = false;
    let mut values = [0.0; ROW_SLOTS];
    discrete_values(time, y, p, &mut values)?;
    let mut guarded = [0.0; ROW_SLOTS];
    if !memories_only {
        guarded_values(time, y, p, &mut guarded)?;
    }
    for (d, &target) in DISCRETE_TARGETS.iter().enumerate() {
        if memories_only == DISCRETE_MEMORY_ROW[d] {
            let value = events.overrides.for_memory(target).unwrap_or(values[d]);
            changed |= ev_write(&mut p[target], value);
        }
    }
    if memories_only {
        return Ok(changed);
    }
    for (g, &slot) in GUARDED_TARGETS.iter().enumerate() {
        if ev_is_reinit(slot) || !ev_guard_runs(events, g, time) {
            continue;
        }
        let value = if slot.in_y { guarded[g] } else { events.overrides.for_memory(slot.index).unwrap_or(guarded[g]) };
        changed |= ev_write(slot_mut(y, p, slot), value);
    }
    Ok(changed)
}

/// Copy each active `reinit` into its state at the end of a pass, evaluated
/// on the pass's settled coordinate (MLS 8.3.6); whether a state changed.
fn ev_reinit(events: &EventMemory, time: Scalar, y: &mut State, p: &Parameters) -> Result<bool, SimError> {
    if !GUARDED_TARGETS.iter().any(|&slot| ev_is_reinit(slot)) {
        return Ok(false);
    }
    let mut values = [0.0; ROW_SLOTS];
    guarded_values(time, y, p, &mut values)?;
    let mut changed = false;
    for (g, &slot) in GUARDED_TARGETS.iter().enumerate() {
        if ev_is_reinit(slot) && ev_guard_runs(events, g, time) {
            changed |= ev_write(&mut y[slot.index], values[g]);
        }
    }
    Ok(changed)
}

/// Run a block of assignment rows to its fixed point: every row reads the
/// same coordinate, then all write.
fn ev_rows(rows: RowFn, targets: &[Slot], time: Scalar, y: &mut State, p: &mut Parameters) -> Result<bool, SimError> {
    if targets.is_empty() {
        return Ok(false);
    }
    let mut changed = false;
    for _ in 0..FIXED_POINT_CAP {
        let mut values = [0.0; ROW_SLOTS];
        rows(time, y, p, &mut values)?;
        let mut round = false;
        for (k, &slot) in targets.iter().enumerate() {
            round |= ev_write(slot_mut(y, p, slot), values[k]);
        }
        if !round {
            return Ok(changed);
        }
        changed = true;
    }
    Err(SimError::EventIteration)
}

fn ev_runtime_assignments(time: Scalar, y: &mut State, p: &mut Parameters) -> Result<bool, SimError> {
    ev_rows(runtime_values, RUNTIME_TARGETS, time, y, p)
}

fn ev_pre_source(y: &State, p: &Parameters, lane: usize) -> f64 {
    let lane = PRE_LANES[lane];
    if lane.in_y { y[lane.source] } else { p[lane.source] }
}

/// The next pass reads the previous pass's values.
fn ev_advance_lanes(y: &State, p: &mut Parameters) -> bool {
    let mut current = [0.0; PRE_LANE_SLOTS];
    for (k, &lane) in ITERATION_LANES.iter().enumerate() {
        current[k] = ev_pre_source(y, p, lane);
    }
    let mut changed = false;
    for (k, &lane) in ITERATION_LANES.iter().enumerate() {
        changed |= ev_write(&mut p[PRE_LANES[lane].dest], current[k]);
    }
    changed
}

fn ev_lanes_settled(y: &State, p: &Parameters) -> bool {
    ITERATION_LANES
        .iter()
        .all(|&lane| p[PRE_LANES[lane].dest].to_bits() == ev_pre_source(y, p, lane).to_bits())
}

/// The discrete settle pass (inner fixed point), as the schedule lists it.
fn ev_discrete_settle(
    events: &EventMemory,
    time: Scalar,
    y: &mut State,
    p: &mut Parameters,
    changed_any: &mut bool,
) -> Result<(), SimError> {
    for _ in 0..FIXED_POINT_CAP {
        let mut pass_changed = false;
        for step in SETTLE_PASS {
            match step {
                SettleStep::DiscreteOwners => pass_changed |= ev_discrete_owners(events, time, y, p, false)?,
                SettleStep::RuntimeAssignments => pass_changed |= ev_runtime_assignments(time, y, p)?,
                SettleStep::SettledIfUnchanged if !pass_changed => return Ok(()),
                SettleStep::SettledIfUnchanged => {}
                SettleStep::AlgebraicProjection => pass_changed |= ev_projection(time, y, p)?,
            }
        }
        *changed_any = true;
    }
    Err(SimError::EventIteration)
}

/// The relation pass (middle fixed point): condition equations settle with
/// the discrete equations under one fixed `pre`.
fn ev_relation_settle(
    events: &mut EventMemory,
    time: Scalar,
    y: &mut State,
    p: &mut Parameters,
    changed_any: &mut bool,
) -> Result<(), SimError> {
    let mut surface = EvSurface::new();
    for _ in 0..FIXED_POINT_CAP {
        for step in RELATION_PASS {
            match step {
                RelationStep::DiscreteSettle => ev_discrete_settle(events, time, y, p, changed_any)?,
                RelationStep::SettledIfRelationsUnchanged => {
                    if !ev_relation_refresh(events, time, y, p, false)? {
                        return Ok(());
                    }
                    *changed_any = true;
                }
                RelationStep::AlgebraicProjection => {
                    ev_projection(time, y, p)?;
                }
                RelationStep::RuntimeAssignments => {
                    ev_runtime_assignments(time, y, p)?;
                }
            }
        }
        if surface.repeats(y, p) {
            if !ev_mode_search(events, &surface, time, y, p)? {
                return Ok(());
            }
            surface.clear();
            *changed_any = true;
        }
    }
    Err(SimError::EventIteration)
}

/// The last `SURFACE_WINDOW` passes' coordinates. An iteration that returns
/// to one of them with the same discrete values is cycling on a relation
/// surface.
struct EvSurface {
    y: [State; SURFACE_SLOTS],
    p: [Parameters; SURFACE_SLOTS],
    count: usize,
    head: usize,
}

impl EvSurface {
    const fn new() -> Self {
        Self { y: [[0.0; Y_LEN]; SURFACE_SLOTS], p: [[0.0; P_LEN]; SURFACE_SLOTS], count: 0, head: 0 }
    }

    fn clear(&mut self) {
        self.count = 0;
        self.head = 0;
    }

    /// Whether this pass repeats a recorded one; records it.
    fn repeats(&mut self, y: &State, p: &Parameters) -> bool {
        if SURFACE_WINDOW == 0 {
            return false;
        }
        let repeats = (0..self.count).any(|k| {
            self.p[k].iter().zip(p).all(|(a, b)| a.to_bits() == b.to_bits())
                && self.y[k].iter().zip(y).all(|(a, b)| !ev_value_changed(*a, *b))
        });
        self.y[self.head] = *y;
        self.p[self.head] = *p;
        self.head = (self.head + 1) % SURFACE_WINDOW;
        if self.count < SURFACE_WINDOW {
            self.count += 1;
        }
        repeats
    }
}

/// The relations to search: those whose memory changed over the recorded
/// passes, and every relation reading a coordinate one of them reads.
fn ev_mode_candidates(surface: &EvSurface, p: &Parameters, candidates: &mut [usize; MODE_SLOTS]) -> usize {
    let mut cycling = [false; ROOT_SLOTS];
    let mut joined = [false; ROOT_SLOTS];
    for root in 0..N_ROOTS {
        if let Some(memory) = ROOT_MEMORY[root] {
            cycling[root] = (0..surface.count).any(|k| surface.p[k][memory].to_bits() != p[memory].to_bits());
        }
    }
    for root in 0..N_ROOTS {
        if cycling[root] {
            for &neighbor in &NEIGHBOR[NEIGHBOR_OFFSET[root]..NEIGHBOR_OFFSET[root + 1]] {
                joined[neighbor] = true;
            }
        }
    }
    let mut count = 0;
    for root in 0..N_ROOTS {
        if joined[root] && ROOT_MEMORY[root].is_some() {
            if count < MODE_LIMIT {
                candidates[count] = root;
            }
            count += 1;
        }
    }
    count
}

/// Whether assigning `mode`'s sides to the candidates gives a coordinate on
/// which each candidate relation takes its assigned side. A mode whose
/// assignments or projection fail is inconsistent; a failing root is an error.
fn ev_mode_consistent(
    time: Scalar,
    y: &mut State,
    p: &mut Parameters,
    candidates: &[usize],
    mode: usize,
) -> Result<bool, SimError> {
    let side = |k: usize| if mode >> k & 1 == 1 { 1.0 } else { 0.0 };
    for (k, &root) in candidates.iter().enumerate() {
        if let Some(memory) = ROOT_MEMORY[root] {
            p[memory] = side(k);
        }
    }
    // The runtime assignments alias relation memory; evaluate them in this
    // mode before projecting it (relation_memory.rs mode_is_consistent).
    if ev_runtime_assignments(time, y, p).is_err() || refresh_algebraics(time, y, p).is_err() {
        return Ok(false);
    }
    let roots = ev_roots(time, y, p)?;
    Ok(candidates.iter().enumerate().all(|(k, &root)| {
        let value = ev_orient_root(roots[root], ROOT_ZERO[root]);
        value == 0.0 || ev_memory_value(value) == side(k)
    }))
}

/// The first joint mode of the cycling relations that is consistent, pinned
/// as overrides; `false` when none is, which keeps the current side.
fn ev_mode_search(
    events: &mut EventMemory,
    surface: &EvSurface,
    time: Scalar,
    y: &mut State,
    p: &mut Parameters,
) -> Result<bool, SimError> {
    let mut candidates = [0; MODE_SLOTS];
    let count = ev_mode_candidates(surface, p, &mut candidates);
    if count == 0 || count > MODE_LIMIT {
        // The iteration cycles among more relations than the search covers.
        return Err(SimError::EventIteration);
    }
    let candidates = &candidates[..count];
    let (base_y, base_p) = (*y, *p);
    for mode in 0..(1_usize << count) {
        if ev_mode_consistent(time, y, p, candidates, mode)? {
            events.overrides.retain(|root, _| !candidates.contains(&root));
            for (c, &root) in candidates.iter().enumerate() {
                events.overrides.push(root, if mode >> c & 1 == 1 { 1.0 } else { 0.0 });
            }
            return Ok(true);
        }
        *y = base_y;
        *p = base_p;
    }
    Ok(false)
}

/// One event pass (outer fixed point), as the schedule lists it.
fn ev_pass(
    events: &mut EventMemory,
    time: Scalar,
    y: &mut State,
    p: &mut Parameters,
    changed: &mut bool,
) -> Result<(), SimError> {
    for step in EVENT_PASS {
        let step_changed = match step {
            EventStep::RelationOverrides => ev_apply_overrides(events, p),
            EventStep::RuntimeAssignments => ev_runtime_assignments(time, y, p)?,
            EventStep::AlgebraicProjection => ev_projection(time, y, p)?,
            EventStep::RelationRefresh => {
                ev_relation_refresh(events, time, y, p, false)? && {
                    ev_runtime_assignments(time, y, p)?;
                    true
                }
            }
            EventStep::RelationSettle => {
                ev_relation_settle(events, time, y, p, changed)?;
                false
            }
        };
        *changed |= step_changed;
    }
    Ok(())
}

/// The event passes: `pre` seeded from the event entry, each later pass
/// reading the previous one, until nothing changes. Then `pre` commits and,
/// with `canonicalize`, the post-commit view settles.
fn ev_iterate_passes(
    events: &mut EventMemory,
    time: Scalar,
    y: &mut State,
    p: &mut Parameters,
    canonicalize: bool,
) -> Result<(), SimError> {
    for lane in 0..PRE_LANES.len() {
        p[PRE_LANES[lane].dest] = ev_pre_source(&events.entry_y, &events.entry_p, lane);
    }
    let mut settled = false;
    let mut surface = EvSurface::new();
    for pass in 0..FIXED_POINT_CAP {
        events.pass = pass;
        let mut changed = pass > 0 && ev_advance_lanes(y, p);
        ev_pass(events, time, y, p, &mut changed)?;
        changed |= ev_reinit(events, time, y, p)?;
        // Condition memories advance on the settled pass.
        changed |= ev_discrete_owners(events, time, y, p, true)?;
        settled = !changed && ev_lanes_settled(y, p);
        if settled {
            break;
        }
        if surface.repeats(y, p) {
            if !ev_mode_search(events, &surface, time, y, p)? {
                settled = true;
                break;
            }
            surface.clear();
        }
    }
    if !settled {
        return Err(SimError::EventIteration);
    }
    // Actions read each condition memory at its event-entry value.
    let mut held = *p;
    for &memory in CONDITION_MEMORIES {
        held[memory] = events.entry_p[memory];
    }
    ev_check_assertions(time, y, &held)?;
    ev_release_pulses(time, y, p)?;
    let committed_p = *p;
    let mut history = false;
    for lane in 0..PRE_LANES.len() {
        let value = ev_pre_source(y, &committed_p, lane);
        history |= ev_value_changed(p[PRE_LANES[lane].dest], value);
        p[PRE_LANES[lane].dest] = value;
    }
    if !canonicalize || !history {
        return Ok(());
    }
    for _ in 0..FIXED_POINT_CAP {
        ev_rows(post_commit_values, POST_COMMIT_TARGETS, time, y, p)?;
        ev_projection(time, y, p)?;
        let relations = ev_relation_refresh(events, time, y, p, true)?;
        // The coordinate is certified; its derived rows must agree with it.
        let changed = ev_rows(post_commit_values, POST_COMMIT_TARGETS, time, y, p)?;
        if !changed && !relations {
            return Ok(());
        }
    }
    Err(SimError::EventIteration)
}

/// Remember the discrete inputs the event just settled with.
fn ev_inputs_seen(events: &mut EventMemory, p: &Parameters) {
    let mut seen = 0;
    for &(base, count) in DISCRETE_INPUTS {
        events.inputs_seen[seen..seen + count].copy_from_slice(&p[base..base + count]);
        seen += count;
    }
}

/// Whether the host changed a discrete input since the latest event: a
/// discrete-time input changes only at events (MLS §4.5).
fn ev_inputs_changed(events: &EventMemory, p: &Parameters) -> bool {
    let mut seen = 0;
    for &(base, count) in DISCRETE_INPUTS {
        let changed = events.inputs_seen[seen..seen + count]
            .iter()
            .zip(&p[base..base + count])
            .any(|(was, now)| was.to_bits() != now.to_bits());
        if changed {
            return true;
        }
        seen += count;
    }
    false
}

/// Fail on the first error-level assertion that does not hold at `time`.
fn ev_check_assertions(time: Scalar, y: &State, p: &Parameters) -> Result<(), SimError> {
    if !ACTIONS.contains(&ActionKind::Error) {
        return Ok(());
    }
    let mut y = *y;
    refresh_algebraics(time, &mut y, p)?;
    let mut values = [0.0; ACTION_SLOTS];
    action_values(time, &y, p, &mut values)?;
    match ACTIONS.iter().zip(values).position(|(&kind, value)| kind == ActionKind::Error && value != 0.0) {
        Some(index) => Err(SimError::Assertion(index)),
        None => Ok(()),
    }
}

/// `SlidingModes` of `rumoca-ir-solve` (`fmi/root_location/sliding.rs`): when
/// both sides of a surface point back at it, every located event re-crosses
/// it at once, so an indicator whose latest `SLIDING_STREAK` events were each
/// within `SLIDING_GAP` of the last applies its changes at the step end until
/// a step passes without one.
#[derive(Clone, Copy, Debug)]
struct SlidingModes {
    last_event: [f64; INDICATOR_SLOTS],
    streak: [u32; INDICATOR_SLOTS],
    sliding: [bool; INDICATOR_SLOTS],
    intervals_since_event: u32,
}

impl SlidingModes {
    const fn new() -> Self {
        Self {
            last_event: [f64::NEG_INFINITY; INDICATOR_SLOTS],
            streak: [0; INDICATOR_SLOTS],
            sliding: [false; INDICATOR_SLOTS],
            intervals_since_event: 0,
        }
    }

    /// The second step begun since the latest state event ends every slide.
    fn begin_interval(&mut self) {
        self.intervals_since_event = self.intervals_since_event.saturating_add(1);
        if self.intervals_since_event == 2 {
            self.sliding = [false; INDICATOR_SLOTS];
        }
    }

    /// Whether the indicators `z` changes are all sliding, and there is one.
    fn only_sliding(&self, events: &EventMemory, z: &[f64; INDICATOR_SLOTS]) -> bool {
        let mut any = false;
        for k in 0..INDICATORS {
            if events.domain[k] != (z[k] > 0.0) {
                if !self.sliding[k] {
                    return false;
                }
                any = true;
            }
        }
        any
    }

    fn record_event(&mut self, time: f64, changed: &[bool; INDICATOR_SLOTS]) {
        for k in 0..INDICATORS {
            if !changed[k] {
                self.streak[k] = 0;
                self.sliding[k] = false;
                continue;
            }
            let close = self.sliding[k] || time - self.last_event[k] <= SLIDING_GAP;
            self.streak[k] = if close { self.streak[k].saturating_add(1) } else { 1 };
            self.last_event[k] = time;
            self.sliding[k] = self.streak[k] >= SLIDING_STREAK;
        }
        self.intervals_since_event = 0;
    }
}

/// Recompute the discrete rows only a public read observes (owners nothing
/// else reads), alternating with the algebraic refresh while they read it.
fn ev_observe(time: Scalar, y: &mut State, p: &mut Parameters) -> Result<(), SimError> {
    if !DISCRETE_OBSERVED_ROW.iter().any(|&observed| observed) {
        return Ok(());
    }
    for _ in 0..FIXED_POINT_CAP {
        let mut values = [0.0; ROW_SLOTS];
        discrete_values(time, y, p, &mut values)?;
        let mut changed = false;
        for (d, &target) in DISCRETE_TARGETS.iter().enumerate() {
            if DISCRETE_OBSERVED_ROW[d] {
                changed |= ev_write(&mut p[target], values[d]);
            }
        }
        refresh_algebraics(time, y, p)?;
        if !changed || !OBSERVATION_READS_Y {
            return Ok(());
        }
    }
    Err(SimError::EventIteration)
}

/// Release the pulse condition memories (`sample` and similar): evaluate
/// their rows on the settled coordinate with no clock ticking.
fn ev_release_pulses(time: Scalar, y: &State, p: &mut Parameters) -> Result<(), SimError> {
    if PULSE_ROWS.is_empty() {
        return Ok(());
    }
    let held = *p;
    clear_clock_lanes(p);
    let mut values = [0.0; ROW_SLOTS];
    let status = pulse_values(time, y, p, &mut values);
    for clock in CLOCKS {
        p[clock.activation] = held[clock.activation];
    }
    status?;
    for &row in PULSE_ROWS {
        p[DISCRETE_TARGETS[row]] = values[row];
    }
    Ok(())
}

/// One event iteration: every clock ticking at `time` reads active for the
/// whole iteration, and none once it has settled.
fn ev_iterate(
    events: &mut EventMemory,
    time: Scalar,
    y: &mut State,
    p: &mut Parameters,
    canonicalize: bool,
) -> Result<(), SimError> {
    write_clock_lanes(p, time);
    let status = ev_iterate_passes(events, time, y, p, canonicalize);
    clear_clock_lanes(p);
    events.pass = 0;
    status
}

/// Seed relation memory and `pre` from the start coordinate, before the
/// initial event iteration.
fn ev_initialize(events: &mut EventMemory, time: Scalar, y: &mut State, p: &mut Parameters) -> Result<(), SimError> {
    if let Some(initial) = INITIAL_EVENT_P {
        p[initial] = 1.0;
    }
    clear_clock_lanes(p);
    events.overrides.clear();
    refresh_algebraics(time, y, p)?;
    ev_relation_refresh(events, time, y, p, false)?;
    for lane in 0..PRE_LANES.len() {
        p[PRE_LANES[lane].dest] = ev_pre_source(y, p, lane);
    }
    ev_discrete_owners(events, time, y, p, true)?;
    events.capture_entry(y, p);
    events.initial_event = true;
    events.domain_valid = false;
    ev_inputs_seen(events, p);
    Ok(())
}

/// The static instant `time` matches, unless a clock ticks there: an event
/// there is observed at the instant's right limit.
fn ev_static_instant(time: Scalar) -> Option<Scalar> {
    if CLOCKS.iter().any(|clock| clock_active(clock, time)) {
        return None;
    }
    TIME_EVENTS.iter().copied().find(|&instant| ev_time_match(instant, time))
}

/// The time the model reads at a reached `time`: the right limit of the
/// static instant it matches, which has fired.
fn ev_now(time: Scalar) -> Scalar {
    ev_static_instant(time).map_or(time, |instant| time.max(instant.next_up()))
}

/// The event at `time`: the initial event, the state event `ev_completed`
/// captured, or (no captured entry) a time event.
fn ev_update(events: &mut EventMemory, time: Scalar, y: &mut State, p: &mut Parameters) -> Result<(), SimError> {
    let initial = events.initial_event;
    if !events.entry_captured {
        refresh_algebraics(time, y, p)?;
        events.capture_entry(y, p);
    }
    let mut status = ev_iterate(events, time, y, p, !initial);
    if status.is_ok() && !initial && ev_static_instant(time).is_some() {
        // The right limit observes the same event from the entry values.
        *y = events.entry_y;
        *p = events.entry_p;
        status = ev_iterate(events, ev_now(time), y, p, true);
    }
    if status.is_ok() {
        ev_inputs_seen(events, p);
    }
    events.entry_captured = false;
    if let Some(index) = INITIAL_EVENT_P {
        if initial && status.is_ok() {
            p[index] = 0.0;
        }
    }
    events.initial_event = false;
    events.overrides.clear();
    status
}

/// Refresh the settled coordinate's algebraics and store its indicator
/// domains.
fn ev_continuous(events: &mut EventMemory, time: Scalar, y: &mut State, p: &Parameters) -> Result<(), SimError> {
    if INDICATORS == 0 {
        refresh_algebraics(time, y, p)?;
    }
    let mut z = [0.0; INDICATOR_SLOTS];
    ev_indicators(events, time, y, p, &mut z)?;
    for k in 0..INDICATORS {
        events.domain[k] = z[k] > 0.0;
    }
    events.domain_valid = true;
    Ok(())
}

// ---------------------------------------------------------------------------
// RootBracket: verbatim from rumoca-ir-solve/src/fmi/root_location/bracket.rs
// (only `open` is `pub` here), so this locator narrows a bracket exactly as
// `rumoca sim` and the FMI C component do. A codegen test keeps them equal.
// ---------------------------------------------------------------------------

/// Which end of a bracket the last evaluation moved.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BracketEnd {
    Low,
    High,
}

/// One domain change being narrowed to the location tolerance.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RootBracket {
    low: f64,
    high: f64,
    tolerance: f64,
    /// `tolerance * 2^(n + s - j)` after `j` evaluations.
    schedule: f64,
    /// `n + s`.
    evaluation_bound: usize,
    /// The Illinois weights of the indicator values held at each end.
    low_weight: f64,
    high_weight: f64,
    last_moved: Option<BracketEnd>,
}

/// Doublings that carry any positive finite tolerance past any finite width.
const DOUBLING_LIMIT: usize = 2100;

impl RootBracket {
    pub fn open(low: f64, high: f64, tolerance: f64, slack: u32) -> Self {
        let width = high - low;
        let mut schedule = tolerance;
        let mut halvings = 0_usize;
        while halvings < DOUBLING_LIMIT && schedule < width {
            schedule *= 2.0;
            halvings += 1;
        }
        for _ in 0..slack {
            schedule *= 2.0;
        }
        Self {
            low,
            high,
            tolerance,
            schedule,
            evaluation_bound: halvings + slack as usize,
            low_weight: 1.0,
            high_weight: 1.0,
            last_moved: None,
        }
    }

    #[must_use]
    pub const fn low(&self) -> f64 {
        self.low
    }

    #[must_use]
    pub const fn high(&self) -> f64 {
        self.high
    }

    /// The evaluations after which the bracket is located, in exact
    /// arithmetic, whatever the indicators.
    #[must_use]
    pub const fn evaluation_bound(&self) -> usize {
        self.evaluation_bound
    }

    /// Whether the bracket is no wider than the location tolerance.
    #[must_use]
    pub fn is_located(&self) -> bool {
        self.high - self.low <= self.tolerance
    }

    /// Where, as a fraction of the bracket from `low`, the Illinois-weighted
    /// secant of one indicator that has entered its new domain at `high`
    /// crosses zero, from its values `at_low` and `at_high` at the two ends.
    ///
    /// The fraction is in `[0, 1]` for any finite values; two values whose
    /// weighted magnitudes do not sum to a positive finite number give the
    /// midpoint.
    #[must_use]
    pub fn crossing_fraction(&self, at_low: f64, at_high: f64) -> f64 {
        let low = self.low_weight * at_low.abs();
        let high = self.high_weight * at_high.abs();
        let sum = low + high;
        if sum > 0.0 && sum.is_finite() {
            low / sum
        } else {
            0.5
        }
    }

    /// The next coordinate to evaluate, from the least
    /// [`crossing_fraction`](Self::crossing_fraction) over the indicators that
    /// have entered their new domain at `high`.
    ///
    /// The secant point is kept half a tolerance inside each end, so a bracket
    /// whose crossing lies within that distance of an end is located by the
    /// next evaluation, then projected onto the minmax radius around the
    /// midpoint. `None` means no representable coordinate lies strictly inside
    /// the bracket: it is as narrow as `f64` allows.
    #[must_use]
    pub fn trial(&self, least_fraction: f64) -> Option<f64> {
        let width = self.high - self.low;
        let middle = self.low + 0.5 * width;
        let secant = (self.low + width * least_fraction)
            .max(self.low + 0.5 * self.tolerance)
            .min(self.high - 0.5 * self.tolerance);
        let radius = (0.5 * self.schedule - 0.5 * width).max(0.0);
        let projected = if secant - middle > radius {
            middle + radius
        } else if middle - secant > radius {
            middle - radius
        } else {
            secant
        };
        if self.low < projected && projected < self.high {
            Some(projected)
        } else if self.low < middle && middle < self.high {
            Some(middle)
        } else {
            None
        }
    }

    /// The end of the [`ToleranceWindow`](super::RootTieBreak::ToleranceWindow)
    /// of a located bracket whose scan sample ends at `upper`: one location
    /// tolerance after the low end, rounded to the nearest coordinate, never
    /// past `upper`.
    ///
    /// The earliest crossing lies in `(low, high]` and `high - low` is within
    /// the tolerance, so the window end is at least `high` and within one
    /// tolerance after the earliest crossing, on its far side.
    #[must_use]
    pub fn application_window(&self, upper: f64) -> f64 {
        (self.low + self.tolerance).min(upper)
    }

    /// Narrow to the evaluation at `trial`: it becomes `high` when an indicator
    /// had entered its new domain there and `low` otherwise. An end retained
    /// twice in a row has its weight halved (the Illinois step), and the end
    /// just moved restarts at weight one.
    pub fn narrow(&mut self, trial: f64, entered: bool) {
        let moved = if entered {
            self.high = trial;
            self.high_weight = 1.0;
            BracketEnd::High
        } else {
            self.low = trial;
            self.low_weight = 1.0;
            BracketEnd::Low
        };
        if self.last_moved == Some(moved) {
            match moved {
                BracketEnd::High => self.low_weight *= 0.5,
                BracketEnd::Low => self.high_weight *= 0.5,
            }
        }
        self.last_moved = Some(moved);
        self.schedule *= 0.5;
    }
}
