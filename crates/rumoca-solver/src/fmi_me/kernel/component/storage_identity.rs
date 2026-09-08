use super::{IndicatorReading, IndicatorZeroSide};

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct IndicatorStorageIdentity {
    pub(super) linked_facts: usize,
    pub(super) entry_table: usize,
    pub(super) entries: Vec<(IndicatorReading, IndicatorZeroSide, Option<usize>)>,
    pub(super) role_widths: (usize, usize, usize, usize),
    pub(super) buffers: [(usize, usize); 8],
}

impl IndicatorStorageIdentity {
    pub(crate) fn role_widths(&self) -> (usize, usize, usize, usize) {
        self.role_widths
    }

    pub(crate) fn buffer_identities(&self) -> &[(usize, usize); 8] {
        &self.buffers
    }
}

/// The `(pointer, length)` identity of every construction-reserved
/// caller-publication buffer used by the derivative getters: the
/// state-derivative output, the directional full seed, the directional state
/// sensitivities, and the directional serialization buffer.
///
/// The buffers are reserved once by instantiation. An equality assertion across
/// real `fmi3GetContinuousStateDerivatives` and `fmi3GetDirectionalDerivative`
/// calls is bounded identity evidence only for a witness whose four widths are
/// nonzero; a zero-width `(pointer, length)` pair is explicitly non-evidence.
/// For that nonzero witness, replacement or growth of these publication
/// buffers changes a pointer or length. This identity makes no transitive
/// allocation claim about evaluator, JVP, or delay workspaces, and it remains
/// blind to shrink-then-regrow reuse at the same address. Those limits are
/// explicit rather than assigned to this identity witness.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct DerivativeScratchIdentity {
    pub(super) output: (usize, usize),
    pub(super) directional_seed: (usize, usize),
    pub(super) directional_sensitivity: (usize, usize),
    pub(super) directional_serialized: (usize, usize),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct EventStageStorageIdentity {
    pub(super) live_model: usize,
    pub(super) stage_model: usize,
    pub(super) live_linked_facts: usize,
    pub(super) stage_linked_facts: usize,
    pub(super) linked_descriptor_table: usize,
    pub(super) live_entry_table: usize,
    pub(super) stage_entry_table: usize,
    pub(super) stage_body: usize,
    pub(super) live_indicator_buffers: [(usize, usize); 8],
    pub(super) stage_indicator_buffers: [(usize, usize); 8],
    pub(super) live_event_buffers: [usize; 9],
    pub(super) stage_event_buffers: [usize; 9],
    pub(super) live_transaction_buffers: [usize; 16],
    pub(super) stage_transaction_buffers: [usize; 16],
}

impl EventStageStorageIdentity {
    pub(crate) fn shares_construction_facts(&self) -> bool {
        self.live_model == self.stage_model
            && self.live_linked_facts == self.stage_linked_facts
            && self.live_entry_table == self.stage_entry_table
    }

    pub(crate) fn same_construction_object(&self, other: &Self) -> bool {
        self.live_model == other.live_model
            && self.stage_model == other.stage_model
            && self.live_linked_facts == other.live_linked_facts
            && self.stage_linked_facts == other.stage_linked_facts
            && self.linked_descriptor_table == other.linked_descriptor_table
            && self.live_entry_table == other.live_entry_table
            && self.stage_entry_table == other.stage_entry_table
            && self.stage_body == other.stage_body
            && self.live_indicator_buffers == other.live_indicator_buffers
            && self.stage_indicator_buffers == other.stage_indicator_buffers
            && self.live_event_buffers == other.live_event_buffers
            && self.stage_event_buffers == other.stage_event_buffers
            && self.live_transaction_buffers == other.live_transaction_buffers
            && self.stage_transaction_buffers == other.stage_transaction_buffers
    }
}
