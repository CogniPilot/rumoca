//! Boolean periodic-task eligibility is separate from MLS §16 ownership.

use super::{ConditionNode, Storage};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct ClockProjection {
    clock: Option<u32>,
    mixed: bool,
    off_tick: Option<bool>,
    on_tick: Option<bool>,
}

impl ClockProjection {
    pub(super) fn issue(node: ConditionNode, storage: &Storage) -> Self {
        let child = |index: u32| storage.conditions[index as usize].clock_projection;
        match node {
            ConditionNode::Clock(clock) => Self {
                clock: Some(clock),
                off_tick: Some(false),
                on_tick: Some(true),
                ..Self::default()
            },
            ConditionNode::Always => Self {
                off_tick: Some(true),
                on_tick: Some(true),
                ..Self::default()
            },
            ConditionNode::Not(inner) => {
                let inner = child(inner);
                Self {
                    off_tick: inner.off_tick.map(|value| !value),
                    on_tick: inner.on_tick.map(|value| !value),
                    ..inner
                }
            }
            ConditionNode::And { lhs, rhs } => Self::combine(child(lhs), child(rhs), false),
            ConditionNode::Or { lhs, rhs } | ConditionNode::AnyRise { lhs, rhs } => {
                Self::combine(child(lhs), child(rhs), true)
            }
            ConditionNode::Initial | ConditionNode::Relation(_) | ConditionNode::Discrete(_) => {
                Self::default()
            }
        }
    }

    fn combine(left: Self, right: Self, disjunction: bool) -> Self {
        let mixed = left.mixed
            || right.mixed
            || matches!((left.clock, right.clock), (Some(a), Some(b)) if a != b);
        if mixed {
            return Self {
                mixed: true,
                ..Self::default()
            };
        }
        Self {
            clock: left.clock.or(right.clock),
            mixed: false,
            off_tick: boolean_value(left.off_tick, right.off_tick, disjunction),
            on_tick: boolean_value(left.on_tick, right.on_tick, disjunction),
        }
    }

    pub(super) fn requires_clock(self, expected: u32) -> bool {
        !self.mixed
            && self.off_tick == Some(false)
            && self.clock.is_none_or(|clock| clock == expected)
    }

    pub(super) fn true_on_tick(self, expected: u32) -> bool {
        !self.mixed
            && self.on_tick == Some(true)
            && self.clock.is_none_or(|clock| clock == expected)
    }

    pub(super) fn selected_clock(self) -> Option<u32> {
        (!self.mixed && self.off_tick == Some(false))
            .then_some(self.clock)
            .flatten()
    }
}

fn boolean_value(left: Option<bool>, right: Option<bool>, disjunction: bool) -> Option<bool> {
    match (left, right, disjunction) {
        (Some(true), _, true) | (_, Some(true), true) => Some(true),
        (Some(false), _, false) | (_, Some(false), false) => Some(false),
        (Some(left), Some(right), true) => Some(left || right),
        (Some(left), Some(right), false) => Some(left && right),
        _ => None,
    }
}
