//! Finalized DAE arenas whose view is built from a single entry.
//!
//! Each family here has two ways in. A single-entry accessor resolves one
//! branded identity, and a walk hands out every entry in storage order paired
//! with the identity of the slot it came from.
//!
//! The walk exists so that a consumer never has to build a dense index. Reading
//! an arena through `0..count()` costs two run-time assertions per step, one
//! that the index has an identity and one that the identity resolves, and both
//! restate what the arena already is. A walk mints the identity from the same
//! step that yields the entry, so neither fact has anywhere left to be asserted:
//! SPEC_0008's justification strings are replaced by the shape of the iterator.

use super::*;

/// Walk of one finalized arena, pairing each entry with its raw slot index.
///
/// The index is minted alongside the entry rather than supplied by a caller,
/// which is what makes the paired identity resolve by construction.
pub(crate) struct ArenaWalk<'dae, T> {
    next_raw: u32,
    entries: std::slice::Iter<'dae, T>,
}

impl<'dae, T> ArenaWalk<'dae, T> {
    pub(crate) fn new(entries: &'dae [T]) -> Self {
        Self {
            next_raw: 0,
            entries: entries.iter(),
        }
    }
}

impl<'dae, T> Iterator for ArenaWalk<'dae, T> {
    type Item = (u32, &'dae T);

    fn next(&mut self) -> Option<Self::Item> {
        let entry = self.entries.next()?;
        let raw = self.next_raw;
        // Construction caps every arena at `u32::MAX` entries, so a walk that
        // starts at zero reaches the last slot without leaving the range.
        self.next_raw = self.next_raw.wrapping_add(1);
        Some((raw, entry))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.entries.size_hint()
    }
}

impl<T> ExactSizeIterator for ArenaWalk<'_, T> {}

fn model_event_transaction_view(
    entry: &crate::model_event_transactions::ModelEventTransactionEntry,
) -> crate::ModelEventTransactionView<'_> {
    crate::ModelEventTransactionView { entry }
}

fn relation_view(entry: &RelationEntry) -> RelationView<'_> {
    RelationView {
        expression: ExprId::from_raw(entry.expression),
        provenance: entry.provenance,
    }
}

fn structured_root_view(entry: &StructuredRootEntry) -> StructuredRootView<'_> {
    StructuredRootView {
        domain: DomainId::from_raw(entry.domain),
        expression: ExprId::from_raw(entry.expression),
        provenance: entry.provenance,
    }
}

fn condition_view(entry: &ConditionEntry) -> ConditionView<'_> {
    let operation = match entry
        .node
        .as_ref()
        .expect("final DAE cannot contain an undefined condition")
    {
        crate::conditions::ConditionNode::Initial => ConditionOperation::Initial,
        crate::conditions::ConditionNode::Always => ConditionOperation::Always,
        crate::conditions::ConditionNode::Relation(raw) => {
            ConditionOperation::Relation(RelationId::from_raw(*raw))
        }
        crate::conditions::ConditionNode::Discrete(raw) => {
            ConditionOperation::Discrete(ExprId::from_raw(*raw))
        }
        crate::conditions::ConditionNode::Clock(raw) => {
            ConditionOperation::Clock(ClockId::from_raw(*raw))
        }
        crate::conditions::ConditionNode::Not(raw) => {
            ConditionOperation::Not(ConditionId::from_raw(*raw))
        }
        crate::conditions::ConditionNode::And { lhs, rhs } => {
            ConditionOperation::And(ConditionId::from_raw(*lhs), ConditionId::from_raw(*rhs))
        }
        crate::conditions::ConditionNode::Or { lhs, rhs } => {
            ConditionOperation::Or(ConditionId::from_raw(*lhs), ConditionId::from_raw(*rhs))
        }
        crate::conditions::ConditionNode::AnyRise { lhs, rhs } => {
            ConditionOperation::AnyRise(ConditionId::from_raw(*lhs), ConditionId::from_raw(*rhs))
        }
    };
    ConditionView {
        operation,
        provenance: entry.provenance,
    }
}

fn root_view(entry: &RootEntry) -> RootView<'_> {
    RootView {
        relation: RelationId::from_raw(entry.relation),
        activation: ConditionId::from_raw(entry.activation),
        provenance: entry.provenance,
    }
}

fn time_event_view(entry: &TimeEventEntry) -> TimeEventView<'_> {
    let operation = match &entry.kind {
        TimeEventKind::Static { instant } => TimeEventOperation::Static(instant),
        TimeEventKind::Dynamic { deadline } => {
            TimeEventOperation::Dynamic(ExprId::from_raw(*deadline))
        }
    };
    TimeEventView {
        operation,
        provenance: entry.provenance,
    }
}

fn event_action_view(entry: &EventActionEntry) -> EventActionView<'_> {
    let operation = match entry.kind {
        EventActionKind::Assert { message, level } => EventActionOperation::Assert {
            message: ExprId::from_raw(message),
            level: level.map(ExprId::from_raw),
        },
        EventActionKind::Terminate { message } => EventActionOperation::Terminate {
            message: ExprId::from_raw(message),
        },
        EventActionKind::Reinitialize { state, value } => EventActionOperation::Reinitialize {
            state: StateId::from_raw(state),
            value: ExprId::from_raw(value),
        },
    };
    EventActionView {
        trigger: ConditionId::from_raw(entry.trigger),
        guard: ConditionId::from_raw(entry.guard),
        operation,
        provenance: entry.provenance,
    }
}

fn clock_view(entry: &ClockEntry) -> ClockView<'_> {
    let operation = match &entry.kind {
        crate::clocks::ClockKind::Periodic(lattice) => ClockOperation::Periodic(lattice),
        crate::clocks::ClockKind::Triggered(condition) => {
            ClockOperation::Triggered(ConditionId::from_raw(*condition))
        }
    };
    ClockView {
        operation,
        provenance: entry.provenance,
    }
}

fn clock_ownership_view(entry: &ClockOwnershipEntry) -> ClockOwnershipView<'_> {
    ClockOwnershipView {
        variable: VariableId::from_raw(entry.variable),
        kind: entry.kind,
        clock: ClockId::from_raw(entry.clock),
        sampled: entry.sampled,
        provenance: entry.provenance,
    }
}

fn previous_view(entry: &PreviousEntry) -> PreviousView<'_> {
    PreviousView {
        variable: VariableId::from_raw(entry.variable),
        clock: ClockId::from_raw(entry.clock),
        provenance: entry.provenance,
    }
}

fn terminal_view(entry: &TerminalEntry) -> TerminalView {
    TerminalView {
        provenance: entry.provenance,
    }
}

fn discrete_real_equation_view(entry: &DiscreteRealEquationEntry) -> DiscreteRealEquationView<'_> {
    let activation = match entry.activation {
        crate::equations::DiscreteRealActivationEntry::Always => DiscreteRealActivation::Always,
        crate::equations::DiscreteRealActivationEntry::When { trigger, guard } => {
            DiscreteRealActivation::When {
                trigger: ConditionId::from_raw(trigger),
                guard: ConditionId::from_raw(guard),
            }
        }
    };
    DiscreteRealEquationView {
        residual: ExprId::from_raw(entry.residual),
        activation,
        provenance: entry.provenance,
    }
}

fn initial_discrete_value_view(entry: &InitialDiscreteValueEntry) -> InitialDiscreteValueView<'_> {
    InitialDiscreteValueView {
        target: VariableId::from_raw(entry.target),
        value: ExprId::from_raw(entry.value),
        provenance: entry.provenance,
    }
}

/// Single-entry accessor and walk for each arena addressed by a branded
/// identity.
macro_rules! branded_arena_families {
    ($(
        $(#[$meta:meta])*
        $accessor:ident / $walk:ident : $id:ident -> $view:ty = $make:ident over $field:ident;
    )+) => {
        impl<'dae> DaeView<'dae> {$(
            $(#[$meta])*
            pub fn $accessor(self, id: $id<'dae>) -> Option<$view> {
                self.dae.storage.$field.get(id.index() as usize).map($make)
            }

            $(#[$meta])*
            ///
            /// Walks the arena in storage order, pairing each entry with the
            /// identity of the slot it came from.
            pub fn $walk(self) -> impl ExactSizeIterator<Item = ($id<'dae>, $view)> {
                ArenaWalk::new(&self.dae.storage.$field)
                    .map(|(raw, entry)| ($id::from_raw(raw), $make(entry)))
            }
        )+}
    };
}

branded_arena_families! {
    /// One MLS §8.5 model-event transaction.
    model_event_transaction / model_event_transactions: ModelEventTransactionId
        -> crate::ModelEventTransactionView<'dae>
        = model_event_transaction_view over model_event_transactions;
    /// One checked MLS §3.5 relation that can generate a state event.
    relation / relations: RelationId -> RelationView<'dae> = relation_view over relations;
    /// One tensor-native family of root surfaces over a compact domain.
    structured_root / structured_roots: StructuredRootId -> StructuredRootView<'dae>
        = structured_root_view over structured_roots;
    /// One checked MLS §8.3.5 activation condition.
    condition / conditions: ConditionId -> ConditionView<'dae> = condition_view over conditions;
    /// One monitored scalar root surface.
    root / roots: RootId -> RootView<'dae> = root_view over roots;
    /// One MLS §8.3.6 time event.
    time_event / time_events: TimeEventId -> TimeEventView<'dae> = time_event_view over time_events;
    /// One MLS §8.3.4 event action.
    event_action / event_actions: EventActionId -> EventActionView<'dae>
        = event_action_view over event_actions;
    /// One MLS §16 clock.
    clock / clocks: ClockId -> ClockView<'dae> = clock_view over clocks;
    /// One MLS §16.5 clock ownership of a discrete coordinate.
    clock_ownership / clock_ownerships: ClockOwnershipId -> ClockOwnershipView<'dae>
        = clock_ownership_view over clock_ownerships;
    /// One MLS §16.5 `previous` coordinate.
    previous / previous_values: PreviousId -> PreviousView<'dae>
        = previous_view over previous_values;
    /// One MLS §8.3.6 `terminal()` observation.
    terminal / terminals: TerminalId -> TerminalView = terminal_view over terminals;
}

impl<'dae> DaeView<'dae> {
    /// MLS §8.5 definition of one discrete Real coordinate.
    pub fn discrete_real_equation(self, index: usize) -> Option<DiscreteRealEquationView<'dae>> {
        self.dae
            .storage
            .discrete_real_equations
            .get(index)
            .map(discrete_real_equation_view)
    }

    /// Every discrete Real definition in storage order.
    ///
    /// This arena is addressed by position rather than by a branded identity,
    /// so the walk yields views alone; a consumer that needs the position takes
    /// it from `enumerate`.
    pub fn discrete_real_equations(
        self,
    ) -> impl ExactSizeIterator<Item = DiscreteRealEquationView<'dae>> {
        self.dae
            .storage
            .discrete_real_equations
            .iter()
            .map(discrete_real_equation_view)
    }

    /// MLS §8.6 initialization-instant value of one discrete coordinate.
    pub fn initial_discrete_value(self, index: usize) -> Option<InitialDiscreteValueView<'dae>> {
        self.dae
            .storage
            .initial_discrete_values
            .get(index)
            .map(initial_discrete_value_view)
    }

    /// Every initialization-instant discrete value in storage order.
    pub fn initial_discrete_values(
        self,
    ) -> impl ExactSizeIterator<Item = InitialDiscreteValueView<'dae>> {
        self.dae
            .storage
            .initial_discrete_values
            .iter()
            .map(initial_discrete_value_view)
    }
}
