//! Which declared inputs a same-instant write can carry into an event owner.

use super::ContinuousRefreshOwners;

/// One bit per declared input scalar: whether a write to it can change an
/// operand of any event owner in the same instant.
///
/// The event owners are the consumers the event refresh plans close over
/// (root conditions, discrete rows, runtime, post-commit and guarded
/// assignments, action conditions and messages) together with the dynamic
/// time-event rows, the delay rows, the clock-partition intermediates and the
/// `pre` copies. A bit is set when one of them reads the input's parameter
/// slot or a non-state solver coordinate whose algebraic definition reads it;
/// a continuous state is never input-dependent within one instant.
///
/// The fact is issued with the refresh owners, from the same event-consumer
/// list and dependency queries as their plans, and is not on the wire: owners
/// decoded or rebuilt without it answer [`Self::all`], as does every
/// construction the dependency owner cannot analyse.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InputEventReach {
    reached: Box<[bool]>,
}

impl InputEventReach {
    /// Every input reaches an event owner: the fail-closed answer.
    #[must_use]
    pub fn all(input_scalars: usize) -> Self {
        Self {
            reached: vec![true; input_scalars].into(),
        }
    }

    /// The inputs among `parameter_reads`, the parameter slots the event
    /// owners and their algebraic closure read. Inputs are the slots
    /// `first_input_parameter .. first_input_parameter + input_scalars`.
    #[must_use]
    pub fn from_parameter_reads(
        first_input_parameter: usize,
        input_scalars: usize,
        parameter_reads: impl IntoIterator<Item = usize>,
    ) -> Self {
        let mut reached = vec![false; input_scalars];
        for slot in parameter_reads {
            if let Some(bit) = slot
                .checked_sub(first_input_parameter)
                .and_then(|input| reached.get_mut(input))
            {
                *bit = true;
            }
        }
        Self {
            reached: reached.into(),
        }
    }

    /// Whether a write to input scalar `input` can reach an event owner. An
    /// input this fact does not cover reaches.
    #[must_use]
    pub fn reaches(&self, input: usize) -> bool {
        self.reached.get(input).copied().unwrap_or(true)
    }

    /// The number of input scalars the fact covers.
    #[must_use]
    pub fn len(&self) -> usize {
        self.reached.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.reached.is_empty()
    }
}

impl ContinuousRefreshOwners {
    /// Attach the input reach issued with these owners' event plans.
    #[must_use]
    pub fn with_input_event_reach(mut self, reach: InputEventReach) -> Self {
        self.input_event_reach = Some(reach);
        self
    }

    /// The issued input reach; `None` for owners constructed without one.
    #[must_use]
    pub fn input_event_reach(&self) -> Option<&InputEventReach> {
        self.input_event_reach.as_ref()
    }

    /// Whether a write to input scalar `input` can reach an event owner in
    /// the same instant. Owners without an issued reach answer `true`.
    #[must_use]
    pub fn input_write_reaches_event_owner(&self, input: usize) -> bool {
        self.input_event_reach
            .as_ref()
            .is_none_or(|reach| reach.reaches(input))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_inputs_among_the_parameter_reads_are_marked() {
        let reach = InputEventReach::from_parameter_reads(4, 3, [0, 3, 5, 7, 900]);
        assert_eq!(reach.len(), 3);
        assert!(!reach.reaches(0));
        assert!(reach.reaches(1));
        assert!(!reach.reaches(2));
    }

    #[test]
    fn an_uncovered_input_and_unissued_owners_reach() {
        let reach = InputEventReach::from_parameter_reads(4, 2, []);
        assert!(!reach.reaches(1));
        assert!(reach.reaches(2));
        assert!(InputEventReach::all(2).reaches(0));
        let owners = ContinuousRefreshOwners::default();
        assert!(owners.input_event_reach().is_none());
        assert!(owners.input_write_reaches_event_owner(0));
        let owners = owners.with_input_event_reach(reach);
        assert!(!owners.input_write_reaches_event_owner(0));
        assert!(owners.input_write_reaches_event_owner(2));
    }
}
