//! Real state a host feeds back from one evaluation to the next (SPEC_0040
//! SOLVE-C101).
//!
//! A stateless step reads its previous state from P and computes its next state
//! into Y. A host that steps the model repeatedly moves the next state onto the
//! previous one between evaluations, which moves every state cell once per step
//! although most of a large state is returned unchanged. A carried run pairs a
//! contiguous range of host Y output cells with the equal range of Real
//! external-input P cells that hold the previous value of the same state. The
//! evaluation publishes the run into those P cells instead of into Y, so the
//! next state and the previous state of the following evaluation are the same
//! cells and a value the step returns unchanged is already in place.

mod lanes;
use super::NativeRefreshAssignmentSchedule;
use crate::{ScalarSlot, SolveProblem, SolveVariableStorageRole, SolveVariableValueKind};
pub use lanes::NativeCarriedLane;
use std::ops::Range;

/// One carried run: `scalar_count` host Y cells from `y_start` are published
/// into the P cells from `p_start`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NativeCarriedRun {
    y_start: usize,
    p_start: usize,
    scalar_count: usize,
}

impl NativeCarriedRun {
    #[must_use]
    pub fn y_range(&self) -> Range<usize> {
        self.y_start..self.y_start + self.scalar_count
    }
    #[must_use]
    pub fn p_range(&self) -> Range<usize> {
        self.p_start..self.p_start + self.scalar_count
    }
    #[must_use]
    pub fn scalar_count(&self) -> usize {
        self.scalar_count
    }
}

/// Why a pairing is not a carried state.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NativeCarriedRefusal {
    /// A pair names no cells, or its two ranges differ in length.
    MismatchedRanges { y: Range<usize>, p: Range<usize> },
    /// A Y range reaches past the host Y scalars (a private work slot is not
    /// host storage).
    YOutOfRange { y: Range<usize> },
    /// A P range reaches past the program's P scalars.
    POutOfRange { p: Range<usize> },
    /// A Y cell is not a model output the host reads.
    YNotOutput { y: Range<usize> },
    /// A P cell is not a Real external input without a typed lane.
    PNotRealInput { p: Range<usize> },
    /// Two pairs name the same Y or P cell.
    Overlap,
    /// A declared Integer or Boolean output has a cell no derived output publishes.
    NotDerivedOutput { variable: String },
    /// A declared Integer or Boolean input is not a typed external input.
    NotTypedInput { variable: String },
    /// The cells of a declared pair differ in lane kind, or are Real.
    LaneMismatch { output: String, input: String },
    /// A declared output is not a Y variable, or a declared input not a P
    /// variable, of the problem.
    UnknownVariable { output: String, input: String },
    /// The two variables of a declared pair differ in shape.
    ShapeMismatch { output: String, input: String },
    /// A declared variable has per-element storage, not one contiguous run.
    IndexedStorage { variable: String },
}

impl std::fmt::Display for NativeCarriedRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MismatchedRanges { y, p } => {
                write!(f, "carried ranges Y {y:?} and P {p:?} differ in length")
            }
            Self::YOutOfRange { y } => write!(f, "carried Y range {y:?} is not host Y storage"),
            Self::POutOfRange { p } => write!(f, "carried P range {p:?} is not P storage"),
            Self::YNotOutput { y } => write!(f, "carried Y range {y:?} is not a model output"),
            Self::PNotRealInput { p } => {
                write!(f, "carried P range {p:?} is not a Real external input")
            }
            Self::Overlap => f.write_str("carried ranges overlap"),
            Self::UnknownVariable { output, input } => write!(
                f,
                "carried pair ({output}, {input}) is not a Y output and a P input of the model"
            ),
            Self::ShapeMismatch { output, input } => {
                write!(f, "carried pair ({output}, {input}) differs in shape")
            }
            Self::IndexedStorage { variable } => {
                write!(f, "carried variable {variable} has no contiguous storage")
            }
            Self::NotDerivedOutput { variable } => write!(
                f,
                "carried variable {variable} has a cell no derived output publishes"
            ),
            Self::NotTypedInput { variable } => {
                write!(
                    f,
                    "carried variable {variable} is not a typed external input"
                )
            }
            Self::LaneMismatch { output, input } => write!(
                f,
                "carried pair ({output}, {input}) differs in lane kind or is Real"
            ),
        }
    }
}

impl std::error::Error for NativeCarriedRefusal {}

/// The carried runs of one schedule, in Y order.
///
/// Reference semantics: after a successful evaluation, P cell
/// `p_start + i` of each run holds the value of work Y cell `y_start + i`, and
/// the host Y cells of the run are not written. Every read of a P cell during
/// the evaluation sees the value the host wrote before it.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct NativeCarriedState {
    runs: Vec<NativeCarriedRun>,
    lanes: Vec<NativeCarriedLane>,
}

/// What the host storage admits as carried: the Y cells it reads as model
/// outputs and the Real external-input P cells it writes, as sorted disjoint
/// ranges.
#[derive(Clone, Debug, Default)]
pub struct NativeCarriedHost {
    y_scalars: usize,
    p_scalars: usize,
    y_outputs: Vec<Range<usize>>,
    p_real_inputs: Vec<Range<usize>>,
}

impl NativeCarriedHost {
    /// A host of `y_scalars` Y cells and `p_scalars` P cells that admits the
    /// listed ranges (overlapping or unsorted ranges are normalized).
    #[must_use]
    pub fn new(
        (y_scalars, p_scalars): (usize, usize),
        y_outputs: impl IntoIterator<Item = Range<usize>>,
        p_real_inputs: impl IntoIterator<Item = Range<usize>>,
    ) -> Self {
        Self {
            y_scalars,
            p_scalars,
            y_outputs: merged(y_outputs.into_iter().collect()),
            p_real_inputs: merged(p_real_inputs.into_iter().collect()),
        }
    }

    /// The host of `problem` whose typed input lanes are `schedule`'s.
    #[must_use]
    pub fn of(problem: &SolveProblem, schedule: &NativeRefreshAssignmentSchedule) -> Self {
        let mut y_outputs = Vec::new();
        let mut p_real_inputs = Vec::new();
        for run in &problem.solve_layout.variable_storage_runs {
            match run.base {
                ScalarSlot::Y { index, .. } if run.role == SolveVariableStorageRole::Output => {
                    y_outputs.push(index..index + run.scalar_count);
                }
                ScalarSlot::P { index, .. }
                    if run.role == SolveVariableStorageRole::ExternalInput
                        && run.value_kind == SolveVariableValueKind::Real =>
                {
                    p_real_inputs.push(index..index + run.scalar_count);
                }
                _ => {}
            }
        }
        // A typed input lane replaces its P slot, which holds only a Real view.
        let typed = schedule
            .input_runs()
            .iter()
            .map(|run| run.p_range())
            .collect::<Vec<_>>();
        Self {
            y_scalars: problem.layout.y_scalars(),
            p_scalars: problem.layout.p_scalars(),
            y_outputs: merged(y_outputs),
            p_real_inputs: subtract(merged(p_real_inputs), merged(typed)),
        }
    }
}

fn merged(mut ranges: Vec<Range<usize>>) -> Vec<Range<usize>> {
    ranges.retain(|range| range.start < range.end);
    ranges.sort_by_key(|range| range.start);
    let mut out: Vec<Range<usize>> = Vec::with_capacity(ranges.len());
    for range in ranges {
        match out.last_mut() {
            Some(last) if range.start <= last.end => last.end = last.end.max(range.end),
            _ => out.push(range),
        }
    }
    out
}

/// `whole` without the cells of `holes`; both sorted and disjoint.
fn subtract(whole: Vec<Range<usize>>, holes: Vec<Range<usize>>) -> Vec<Range<usize>> {
    let mut out = Vec::with_capacity(whole.len());
    for mut range in whole {
        for hole in &holes {
            if hole.end <= range.start || range.end <= hole.start {
                continue;
            }
            if range.start < hole.start {
                out.push(range.start..hole.start);
            }
            range.start = range.start.max(hole.end);
        }
        if range.start < range.end {
            out.push(range);
        }
    }
    out
}

fn covers(ranges: &[Range<usize>], cells: &Range<usize>) -> bool {
    let at = ranges.partition_point(|range| range.end <= cells.start);
    ranges
        .get(at)
        .is_some_and(|range| range.start <= cells.start && cells.end <= range.end)
}

impl NativeCarriedState {
    /// The runs, in increasing Y order.
    #[must_use]
    pub fn runs(&self) -> &[NativeCarriedRun] {
        &self.runs
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.runs.is_empty() && self.lanes.is_empty()
    }

    /// The Integer and Boolean lane runs, in increasing output lane order.
    #[must_use]
    pub fn lanes(&self) -> &[NativeCarriedLane] {
        &self.lanes
    }

    /// The lane run holding the output lane byte `offset`, if any.
    #[must_use]
    pub fn lane_of_output(&self, offset: usize) -> Option<&NativeCarriedLane> {
        let at = self
            .lanes
            .partition_point(|lane| lane.output_range().end <= offset);
        self.lanes
            .get(at)
            .filter(|lane| lane.output_range().start <= offset)
    }

    /// The run holding host Y cell `cell`, if any.
    #[must_use]
    pub fn run_of_y(&self, cell: usize) -> Option<&NativeCarriedRun> {
        let at = self.runs.partition_point(|run| run.y_range().end <= cell);
        self.runs.get(at).filter(|run| run.y_range().start <= cell)
    }

    /// Pair `(y, p)` ranges of `host`. Each pair stays one run: a backend decides
    /// per run whether its value is returned unchanged.
    pub fn construct_for_host(
        host: &NativeCarriedHost,
        pairs: &[(Range<usize>, Range<usize>)],
    ) -> Result<Self, NativeCarriedRefusal> {
        let mut runs = Vec::with_capacity(pairs.len());
        for (y, p) in pairs {
            if y.start >= y.end || y.end - y.start != p.end.saturating_sub(p.start) {
                return Err(NativeCarriedRefusal::MismatchedRanges {
                    y: y.clone(),
                    p: p.clone(),
                });
            }
            if y.end > host.y_scalars {
                return Err(NativeCarriedRefusal::YOutOfRange { y: y.clone() });
            }
            if p.end > host.p_scalars {
                return Err(NativeCarriedRefusal::POutOfRange { p: p.clone() });
            }
            if !covers(&host.y_outputs, y) {
                return Err(NativeCarriedRefusal::YNotOutput { y: y.clone() });
            }
            if !covers(&host.p_real_inputs, p) {
                return Err(NativeCarriedRefusal::PNotRealInput { p: p.clone() });
            }
            runs.push(NativeCarriedRun {
                y_start: y.start,
                p_start: p.start,
                scalar_count: y.end - y.start,
            });
        }
        runs.sort_by_key(|run| run.y_start);
        let mut p_order = runs
            .iter()
            .map(NativeCarriedRun::p_range)
            .collect::<Vec<_>>();
        p_order.sort_by_key(|range| range.start);
        let overlapping =
            |ranges: &[Range<usize>]| ranges.windows(2).any(|pair| pair[0].end > pair[1].start);
        if overlapping(
            &runs
                .iter()
                .map(NativeCarriedRun::y_range)
                .collect::<Vec<_>>(),
        ) || overlapping(&p_order)
        {
            return Err(NativeCarriedRefusal::Overlap);
        }
        Ok(Self {
            runs,
            lanes: Vec::new(),
        })
    }

    /// Pair the declared variables of `pairs`, each `(output, input)` naming a
    /// model output and the external input that holds its previous value, both
    /// of one value type and shape. A Real pair is a Y output and a Real input,
    /// published through P; an Integer or Boolean pair is a derived-discrete
    /// output and a typed input lane, published through the lanes (SOLVE-C101).
    /// The pairing is the caller's declaration of a host contract (the host
    /// feeds the output to the input); nothing is inferred from names.
    pub fn declared(
        problem: &SolveProblem,
        schedule: &NativeRefreshAssignmentSchedule,
        pairs: &[(&str, &str)],
    ) -> Result<Self, NativeCarriedRefusal> {
        let layout = &problem.layout;
        let cells = |name: &str| {
            if layout.has_indexed_shape_binding(&rumoca_core::VarName::intern(name)) {
                return Err(NativeCarriedRefusal::IndexedStorage {
                    variable: name.to_owned(),
                });
            }
            let count = layout
                .shape(name)
                .map_or(1, |shape| shape.iter().product::<usize>());
            Ok((layout.binding(name), layout.shape(name), count))
        };
        let derived = lanes::DerivedBySlot::of(schedule);
        let mut ranges = Vec::with_capacity(pairs.len());
        let mut lane_runs = Vec::new();
        for &(output, input) in pairs {
            let (out_slot, out_shape, count) = cells(output)?;
            let (in_slot, in_shape, _) = cells(input)?;
            let unknown = || NativeCarriedRefusal::UnknownVariable {
                output: output.to_owned(),
                input: input.to_owned(),
            };
            let Some(ScalarSlot::P { index: p, .. }) = in_slot else {
                return Err(unknown());
            };
            if out_shape != in_shape {
                return Err(NativeCarriedRefusal::ShapeMismatch {
                    output: output.to_owned(),
                    input: input.to_owned(),
                });
            }
            match out_slot {
                Some(ScalarSlot::Y { index: y, .. }) => ranges.push((y..y + count, p..p + count)),
                Some(ScalarSlot::P { index: out, .. }) => lane_runs.extend(lanes::pair(
                    schedule,
                    &derived,
                    ((out, p), count),
                    (output, input),
                )?),
                _ => return Err(unknown()),
            }
        }
        let mut state =
            Self::construct_for_host(&NativeCarriedHost::of(problem, schedule), &ranges)?;
        lane_runs.sort_by_key(|lane| lane.output_range().start);
        let overlapping = |ranges: Vec<Range<usize>>| lanes::overlap(ranges);
        if overlapping(
            lane_runs
                .iter()
                .map(NativeCarriedLane::output_range)
                .collect(),
        ) || overlapping(
            lane_runs
                .iter()
                .map(NativeCarriedLane::input_range)
                .collect(),
        ) {
            return Err(NativeCarriedRefusal::Overlap);
        }
        state.lanes = lane_runs;
        Ok(state)
    }

    /// Reference publication of the lane runs: write each run of the staged
    /// `output_lanes` into `input_lanes`.
    pub fn publish_lanes(&self, output_lanes: &[u8], input_lanes: &mut [u8]) {
        for lane in &self.lanes {
            input_lanes[lane.input_range()].copy_from_slice(&output_lanes[lane.output_range()]);
        }
    }

    /// Reference publication: write each run of `work_y` into `p`.
    pub fn publish(&self, work_y: &[f64], p: &mut [f64]) {
        for run in &self.runs {
            p[run.p_range()].copy_from_slice(&work_y[run.y_range()]);
        }
    }
}

impl NativeRefreshAssignmentSchedule {
    /// The schedule with `state` as the carried state its evaluations publish
    /// into P.
    #[must_use]
    pub fn with_carried_state(&self, state: NativeCarriedState) -> Self {
        let mut schedule = self.clone();
        schedule.carried = state;
        schedule
    }

    /// The Real state this schedule's evaluations publish into P instead of Y.
    #[must_use]
    pub fn carried_state(&self) -> &NativeCarriedState {
        &self.carried
    }
}

#[cfg(test)]
mod tests;
