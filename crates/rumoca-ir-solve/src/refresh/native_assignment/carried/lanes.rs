//! Integer and Boolean state carried through the typed lanes (SPEC_0040
//! SOLVE-C101).
//!
//! The typed counterpart of a Real carried run: derived-discrete output
//! cells staged in the output lanes are published into the input lane cells
//! that hold the previous value of the same state, in the same byte order and
//! width, so the host moves nothing between evaluations.

use super::{NativeCarriedRefusal, NativeRefreshAssignmentSchedule};
use crate::{NativeDerivedOutput, NativeOutputLane, SolveVariableStorageRole};
use std::collections::BTreeMap;
use std::ops::Range;

/// One carried lane run: `scalar_count` cells of `lane` from byte
/// `output_offset` of the output lanes are published at byte `input_offset` of
/// the input lanes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NativeCarriedLane {
    output_offset: usize,
    input_offset: usize,
    lane: NativeOutputLane,
    scalar_count: usize,
}

impl NativeCarriedLane {
    /// Bytes of the output lanes the run is read from.
    #[must_use]
    pub fn output_range(&self) -> Range<usize> {
        self.output_offset..self.output_offset + self.bytes()
    }
    /// Bytes of the input lanes the run is published to.
    #[must_use]
    pub fn input_range(&self) -> Range<usize> {
        self.input_offset..self.input_offset + self.bytes()
    }
    #[must_use]
    pub fn lane(&self) -> NativeOutputLane {
        self.lane
    }
    #[must_use]
    pub fn scalar_count(&self) -> usize {
        self.scalar_count
    }
    #[must_use]
    pub fn bytes(&self) -> usize {
        self.scalar_count * self.lane.width()
    }
}

/// The derived outputs of a schedule by the Solve P slot they publish.
pub(super) struct DerivedBySlot<'a>(BTreeMap<usize, &'a NativeDerivedOutput>);

impl<'a> DerivedBySlot<'a> {
    pub(super) fn of(schedule: &'a NativeRefreshAssignmentSchedule) -> Self {
        Self(
            schedule
                .derived_outputs()
                .iter()
                .map(|output| (output.p_index(), output))
                .collect(),
        )
    }
}

/// The lane runs that pair the `count` derived-output cells from P slot
/// `output` with the `count` external-input lane cells from P slot `input`,
/// split wherever either side stops being consecutive.
pub(super) fn pair(
    schedule: &NativeRefreshAssignmentSchedule,
    derived: &DerivedBySlot<'_>,
    ((output, input), count): ((usize, usize), usize),
    names: (&str, &str),
) -> Result<Vec<NativeCarriedLane>, NativeCarriedRefusal> {
    let refuse = |input_side: bool| {
        let variable = if input_side { names.1 } else { names.0 };
        if input_side {
            NativeCarriedRefusal::NotTypedInput {
                variable: variable.to_owned(),
            }
        } else {
            NativeCarriedRefusal::NotDerivedOutput {
                variable: variable.to_owned(),
            }
        }
    };
    let mut runs: Vec<NativeCarriedLane> = Vec::new();
    for cell in 0..count {
        let out = derived
            .0
            .get(&(output + cell))
            .ok_or_else(|| refuse(false))?;
        let lane = schedule
            .input_lane(input + cell)
            .filter(|_| {
                schedule.input_runs().iter().any(|run| {
                    run.p_range().contains(&(input + cell))
                        && run.role() == SolveVariableStorageRole::ExternalInput
                })
            })
            .ok_or_else(|| refuse(true))?;
        if lane.lane() != out.lane() || out.lane() == NativeOutputLane::Real {
            return Err(NativeCarriedRefusal::LaneMismatch {
                output: names.0.to_owned(),
                input: names.1.to_owned(),
            });
        }
        match runs.last_mut() {
            Some(run)
                if run.output_offset + run.bytes() == out.lane_offset()
                    && run.input_offset + run.bytes() == lane.lane_offset() =>
            {
                run.scalar_count += 1;
            }
            _ => runs.push(NativeCarriedLane {
                output_offset: out.lane_offset(),
                input_offset: lane.lane_offset(),
                lane: lane.lane(),
                scalar_count: 1,
            }),
        }
    }
    Ok(runs)
}

/// Whether the byte ranges, sorted by start, share a byte.
pub(super) fn overlap(mut ranges: Vec<Range<usize>>) -> bool {
    ranges.sort_by_key(|range| range.start);
    ranges.windows(2).any(|pair| pair[0].end > pair[1].start)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::NativeCarriedState;

    fn lane(
        output_offset: usize,
        input_offset: usize,
        lane: NativeOutputLane,
        scalar_count: usize,
    ) -> NativeCarriedLane {
        NativeCarriedLane {
            output_offset,
            input_offset,
            lane,
            scalar_count,
        }
    }

    /// An Integer run of two cells, then a Boolean run of three cells.
    fn state() -> NativeCarriedState {
        NativeCarriedState {
            runs: Vec::new(),
            lanes: vec![
                lane(8, 0, NativeOutputLane::Integer, 2),
                lane(32, 24, NativeOutputLane::Boolean, 3),
            ],
        }
    }

    #[test]
    fn a_run_spans_its_scalar_count_in_the_width_of_its_lane() {
        let state = state();
        let [integer, boolean] = state.lanes() else {
            panic!("two lane runs")
        };
        assert_eq!((integer.scalar_count(), integer.bytes()), (2, 16));
        assert_eq!((boolean.scalar_count(), boolean.bytes()), (3, 3));
        assert_eq!(integer.output_range(), 8..24);
        assert_eq!(boolean.input_range(), 24..27);
        assert!(!state.is_empty());
    }

    #[test]
    fn the_run_of_an_output_byte_is_found_by_any_byte_inside_it_and_by_no_other() {
        let state = state();
        let at = |offset| state.lane_of_output(offset).map(NativeCarriedLane::lane);
        assert_eq!(at(7), None);
        assert_eq!(at(8), Some(NativeOutputLane::Integer));
        assert_eq!(at(23), Some(NativeOutputLane::Integer));
        assert_eq!(at(24), None);
        assert_eq!(at(31), None);
        assert_eq!(at(34), Some(NativeOutputLane::Boolean));
        assert_eq!(at(35), None);
    }

    #[test]
    fn publication_copies_each_run_into_the_input_lanes_and_leaves_other_bytes() {
        let state = state();
        let output: Vec<u8> = (0..40).collect();
        let mut input = vec![0xee; 32];
        state.publish_lanes(&output, &mut input);
        assert_eq!(input[0..16], output[8..24]);
        assert!(input[16..24].iter().all(|byte| *byte == 0xee));
        assert_eq!(input[24..27], output[32..35]);
        assert!(input[27..].iter().all(|byte| *byte == 0xee));
    }
}
