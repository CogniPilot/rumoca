//! Which root-condition and discrete-row programs an event update may take
//! from their last evaluation in the same event.
//!
//! A typed program is pure in the solver coordinates, parameters and time it
//! reads, and time is fixed inside one event. The issued event owner reach
//! (`solve::EventOwnerReach`) names what each program reads. The journal
//! keeps, for every lane some program reads, its bits at the last
//! observation and the observation at which they last changed; a program
//! keeps the observation it was evaluated at. A program whose lanes all last
//! changed at or before that observation, at the same time and under the
//! same relation overrides, would return the bits it returned then.
//!
//! "Changed" is a change of bits: `+0.0` to `-0.0` is one, a write of the
//! same bits is not. Changes are observed by comparing the lanes themselves,
//! so no writer has to report what it wrote.

#[cfg(test)]
mod tests;

use rumoca_ir_solve as solve;

use super::{
    RuntimeSolveError, ScalarExecutionView, event_update::EventEvalParamCache,
    native_specialization::RowEvalPoint,
};

/// What the root-condition evaluation and each discrete-row program read,
/// as positions into the journal's tracked lanes.
#[derive(Clone, Debug)]
pub(super) struct EventOwnerLanes {
    y: Box<[usize]>,
    p: Box<[usize]>,
    roots: OwnerLanes,
    programs: Box<[OwnerLanes]>,
}

#[derive(Clone, Debug, Default)]
struct OwnerLanes {
    y: Box<[u32]>,
    p: Box<[u32]>,
}

impl EventOwnerLanes {
    /// The lanes of `problem`'s root conditions and discrete rows, or why
    /// its event updates evaluate every owner at every step.
    pub(super) fn for_problem(
        problem: &solve::SolveProblem,
    ) -> Result<Self, solve::EventReachRefusal> {
        let reach = problem.events.owner_reach.checked_for(problem)?;
        reach.restricted_event_update()?;
        let mut roots = (
            solve::IndexIntervals::default(),
            solve::IndexIntervals::default(),
        );
        let mut programs = vec![
            (
                solve::IndexIntervals::default(),
                solve::IndexIntervals::default()
            );
            problem.discrete.rhs.programs().len()
        ];
        let (mut all_y, mut all_p) = (
            solve::IndexIntervals::default(),
            solve::IndexIntervals::default(),
        );
        for (owner, reads) in reach.owners() {
            let lanes = match owner {
                solve::EventOwner::RootCondition(_) => &mut roots,
                solve::EventOwner::DiscreteRow(program) => match programs.get_mut(program) {
                    Some(lanes) => lanes,
                    None => return Err(solve::EventReachRefusal::Mismatch),
                },
                _ => continue,
            };
            lanes.0 = lanes.0.union(reads.y());
            lanes.1 = lanes.1.union(reads.p());
            all_y = all_y.union(reads.y());
            all_p = all_p.union(reads.p());
        }
        let y = all_y.iter().collect::<Box<[usize]>>();
        let p = all_p.iter().collect::<Box<[usize]>>();
        let positions = |tracked: &[usize], lanes: &solve::IndexIntervals| {
            lanes
                .iter()
                .map(|lane| {
                    tracked
                        .binary_search(&lane)
                        .ok()
                        .and_then(|position| u32::try_from(position).ok())
                        .ok_or(solve::EventReachRefusal::Mismatch)
                })
                .collect::<Result<Box<[u32]>, _>>()
        };
        let owner = |(owner_y, owner_p): &(solve::IndexIntervals, solve::IndexIntervals)| {
            Ok(OwnerLanes {
                y: positions(&y, owner_y)?,
                p: positions(&p, owner_p)?,
            })
        };
        let roots = owner(&roots)?;
        let programs = programs
            .iter()
            .map(owner)
            .collect::<Result<Box<[OwnerLanes]>, _>>()?;
        Ok(Self {
            y,
            p,
            roots,
            programs,
        })
    }
}

/// One program's last evaluation in the current event.
#[derive(Clone, Debug, Default)]
struct OwnerEvaluation {
    observation: Option<u64>,
    time: u64,
    overrides: Vec<(usize, u64)>,
    outputs: Vec<f64>,
}

impl OwnerEvaluation {
    fn record(&mut self, observation: u64, time: f64, overrides: &[(usize, f64)], outputs: &[f64]) {
        self.observation = Some(observation);
        self.time = time.to_bits();
        self.overrides.clear();
        self.overrides.extend(
            overrides
                .iter()
                .map(|(root, value)| (*root, value.to_bits())),
        );
        self.outputs.clear();
        self.outputs.extend_from_slice(outputs);
    }

    fn matches(&self, time: f64, overrides: &[(usize, f64)]) -> bool {
        self.time == time.to_bits()
            && self.overrides.len() == overrides.len()
            && self
                .overrides
                .iter()
                .zip(overrides)
                .all(|((root, bits), (other, value))| root == other && *bits == value.to_bits())
    }
}

/// The lanes the issued owners read, as one event update has observed them.
#[derive(Clone, Debug, Default)]
pub(super) struct EventOwnerJournal {
    /// Whether the event update now running takes clean owners from their
    /// last evaluation; decided when the update opens the journal.
    restricted: bool,
    observation: u64,
    y_bits: Vec<u64>,
    p_bits: Vec<u64>,
    /// The observation at which each tracked lane last changed bits; zero for
    /// a lane that has not changed since the update opened the journal, so
    /// the nonzero lanes are the event-wide union of changed lanes.
    y_changed_at: Vec<u64>,
    p_changed_at: Vec<u64>,
    roots: OwnerEvaluation,
    programs: Vec<OwnerEvaluation>,
}

impl EventOwnerJournal {
    /// Start one event update over the coordinate it enters with. Every
    /// owner is evaluated at least once after this.
    pub(super) fn open(&mut self, lanes: Option<&EventOwnerLanes>, y: &[f64], p: &[f64]) {
        self.restricted = false;
        let Some(lanes) = lanes else {
            return;
        };
        let (Some(y_bits), Some(p_bits)) = (bits_of(&lanes.y, y), bits_of(&lanes.p, p)) else {
            return;
        };
        self.restricted = true;
        self.observation = 0;
        self.y_bits = y_bits;
        self.p_bits = p_bits;
        reset(&mut self.y_changed_at, lanes.y.len(), 0);
        reset(&mut self.p_changed_at, lanes.p.len(), 0);
        self.roots = OwnerEvaluation::default();
        self.programs.clear();
        self.programs
            .resize_with(lanes.programs.len(), OwnerEvaluation::default);
    }

    pub(super) fn restricted(&self) -> bool {
        self.restricted
    }

    /// Compare every tracked lane with its bits at the last observation. A
    /// coordinate of another shape than the one the journal opened with ends
    /// the restriction for the rest of the update.
    pub(super) fn observe(&mut self, lanes: &EventOwnerLanes, y: &[f64], p: &[f64]) {
        if !self.restricted {
            return;
        }
        if lanes.y.last().is_some_and(|lane| *lane >= y.len())
            || lanes.p.last().is_some_and(|lane| *lane >= p.len())
        {
            self.restricted = false;
            return;
        }
        self.observation += 1;
        let observation = self.observation;
        for (position, lane) in lanes.y.iter().enumerate() {
            let bits = y[*lane].to_bits();
            if self.y_bits[position] != bits {
                self.y_bits[position] = bits;
                self.y_changed_at[position] = observation;
            }
        }
        for (position, lane) in lanes.p.iter().enumerate() {
            let bits = p[*lane].to_bits();
            if self.p_bits[position] != bits {
                self.p_bits[position] = bits;
                self.p_changed_at[position] = observation;
            }
        }
    }

    fn clean(&self, lanes: &OwnerLanes, evaluation: &OwnerEvaluation) -> bool {
        let Some(evaluated) = evaluation.observation else {
            return false;
        };
        lanes
            .y
            .iter()
            .all(|position| self.y_changed_at[*position as usize] <= evaluated)
            && lanes
                .p
                .iter()
                .all(|position| self.p_changed_at[*position as usize] <= evaluated)
    }

    /// The root values of the last evaluation, when no lane the root
    /// conditions read changed bits since and time is the same.
    pub(super) fn clean_roots(&self, lanes: &EventOwnerLanes, time: f64) -> Option<&[f64]> {
        (self.restricted && self.roots.matches(time, &[]) && self.clean(&lanes.roots, &self.roots))
            .then_some(self.roots.outputs.as_slice())
    }

    /// Record the root values evaluated at the latest observation.
    pub(super) fn record_roots(&mut self, time: f64, values: &[f64]) {
        if self.restricted {
            self.roots.record(self.observation, time, &[], values);
        }
    }

    /// The outputs of discrete program `program` at its last evaluation, when
    /// no lane it reads changed bits since and time and overrides are the
    /// same.
    pub(super) fn clean_program(
        &self,
        lanes: &EventOwnerLanes,
        program: usize,
        time: f64,
        overrides: &[(usize, f64)],
    ) -> Option<&[f64]> {
        let (owner, evaluation) = (lanes.programs.get(program)?, self.programs.get(program)?);
        (self.restricted && evaluation.matches(time, overrides) && self.clean(owner, evaluation))
            .then_some(evaluation.outputs.as_slice())
    }

    /// Record the outputs of discrete program `program` evaluated at the
    /// latest observation.
    pub(super) fn record_program(
        &mut self,
        program: usize,
        time: f64,
        overrides: &[(usize, f64)],
        outputs: &[f64],
    ) {
        let observation = self.observation;
        if self.restricted
            && let Some(evaluation) = self.programs.get_mut(program)
        {
            evaluation.record(observation, time, overrides, outputs);
        }
    }

    /// How many tracked lanes changed bits since the journal opened, as
    /// `[y, p]`.
    #[cfg(test)]
    fn changed_in_event(&self) -> [usize; 2] {
        let changed = |at: &[u64]| at.iter().filter(|observation| **observation != 0).count();
        [changed(&self.y_changed_at), changed(&self.p_changed_at)]
    }
}

impl ScalarExecutionView<'_> {
    /// Open the journal for one event update entered at `y`, `p`: the update
    /// takes clean owners from their last evaluation when this model's
    /// issued reach admits it. Decided here, before anything is evaluated.
    pub(super) fn open_event_owner_journal(&self, y: &[f64], p: &[f64]) {
        #[cfg(any(test, feature = "input-write-audit"))]
        crate::runtime::event_owner_audit::count(|counts| counts.events += 1);
        #[cfg(any(test, feature = "input-write-audit"))]
        let forced = crate::runtime::event_owner_audit::full_event_update_forced();
        #[cfg(not(any(test, feature = "input-write-audit")))]
        let forced = false;
        let lanes = self.event_owner_lanes.as_ref().ok().filter(|_| !forced);
        self.event_owner_journal.borrow_mut().open(lanes, y, p);
    }

    /// [`Self::eval_root_conditions_from_solver_y`] inside an event update:
    /// the values of the last evaluation when no lane the root conditions
    /// read changed bits since and time is the same.
    pub(super) fn eval_event_root_conditions(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let Ok(lanes) = &self.event_owner_lanes else {
            return self.eval_root_conditions_from_solver_y(t, y, p);
        };
        let clean = {
            let mut journal = self.event_owner_journal.borrow_mut();
            journal.observe(lanes, y, p);
            journal.clean_roots(lanes, t).map(<[f64]>::to_vec)
        };
        if let Some(values) = clean {
            #[cfg(any(test, feature = "input-write-audit"))]
            crate::runtime::event_owner_audit::count(|counts| counts.root_evaluations_skipped += 1);
            return Ok(values);
        }
        let values = self.eval_root_conditions_from_solver_y(t, y, p)?;
        self.event_owner_journal
            .borrow_mut()
            .record_roots(t, &values);
        Ok(values)
    }

    /// A row-evaluation cache for one pass that reads `y`, `p` as they are
    /// now; the journal observes them first, so the pass may take clean
    /// discrete programs from their last evaluation.
    pub(super) fn observed_event_eval_param_cache(
        &self,
        y: &[f64],
        p: &[f64],
    ) -> EventEvalParamCache {
        let observed = match &self.event_owner_lanes {
            Ok(lanes) => {
                let mut journal = self.event_owner_journal.borrow_mut();
                journal.observe(lanes, y, p);
                journal.restricted()
            }
            Err(_) => false,
        };
        EventEvalParamCache {
            observed,
            ..EventEvalParamCache::default()
        }
    }

    /// [`Self::eval_discrete_program_outputs`] in an event pass: the outputs
    /// of the program's last evaluation when the pass observed the journal
    /// and no lane the program reads changed bits since, at the same time and
    /// under the same relation overrides.
    pub(super) fn eval_event_discrete_program(
        &self,
        observed: bool,
        program: usize,
        point: RowEvalPoint<'_>,
        overrides: &[(usize, f64)],
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        let lanes = self.event_owner_lanes.as_ref().ok().filter(|_| observed);
        if let Some(lanes) = lanes
            && let Some(outputs) = self
                .event_owner_journal
                .borrow()
                .clean_program(lanes, program, point.t, overrides)
        {
            out.clear();
            out.extend_from_slice(outputs);
            #[cfg(any(test, feature = "input-write-audit"))]
            crate::runtime::event_owner_audit::count(|counts| {
                counts.discrete_programs_skipped += 1
            });
            return Ok(());
        }
        #[cfg(any(test, feature = "input-write-audit"))]
        crate::runtime::event_owner_audit::count(|counts| counts.discrete_programs_evaluated += 1);
        self.eval_discrete_program_outputs(program, point.y, point.p, point.t, out)?;
        if lanes.is_some() {
            self.event_owner_journal
                .borrow_mut()
                .record_program(program, point.t, overrides, out);
        }
        Ok(())
    }
}

fn bits_of(lanes: &[usize], values: &[f64]) -> Option<Vec<u64>> {
    lanes
        .iter()
        .map(|lane| values.get(*lane).map(|value| value.to_bits()))
        .collect()
}

fn reset<T: Copy>(values: &mut Vec<T>, len: usize, value: T) {
    values.clear();
    values.resize(len, value);
}
