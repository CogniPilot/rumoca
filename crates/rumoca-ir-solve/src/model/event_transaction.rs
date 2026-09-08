//! Event-transaction programs: the checked aggregate inputs, targets, and
//! statement bodies that a discrete pass replays atomically.
//!
//! Kept beside [`DiscreteSolveSystem`] so the discrete system stays a shape
//! declaration while the transaction body keeps its own construction rules.

use super::*;

/// One aggregate model-storage input to an event transaction.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct EventTransactionInput {
    source: ScalarSlot,
    value_type: SolveValueType,
}

impl EventTransactionInput {
    #[must_use]
    pub const fn source(&self) -> ScalarSlot {
        self.source
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }
}

/// One aggregate atomic-commit destination of an event transaction.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct EventTransactionTarget {
    base: ScalarSlot,
    value_type: SolveValueType,
    clock_owner: Option<PeriodicClockId>,
}

/// Exact executable producer represented by one aggregate transaction target.
///
/// This is a compiler-issued projection, not a runtime recovery hint. Whole
/// problem validation proves that it defines the same compact storage range as
/// the aligned transaction target and that producer programs are covered as
/// complete execution units.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum EventTransactionProducerOwner {
    ScalarRows {
        start_row: usize,
    },
    StructuredUpdate {
        update_index: usize,
    },
    GuardedAssignment {
        program_index: usize,
        target_range_index: usize,
    },
}

impl EventTransactionTarget {
    #[must_use]
    pub const fn base(&self) -> ScalarSlot {
        self.base
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub const fn clock_owner(&self) -> Option<PeriodicClockId> {
        self.clock_owner
    }
}

/// One checked tensor-native executable model-event transaction.
///
/// The typed body is owned once by `site` in the model pure-call table. Inputs
/// and targets retain aggregate types and compact storage bases; coordinates
/// are materialized only by the final execution or emission adapter.
#[derive(Clone, Debug, Serialize)]
pub struct EventTransactionProgram {
    site: SolvePureCallSite,
    inputs: Box<[EventTransactionInput]>,
    targets: Box<[EventTransactionTarget]>,
    producer_owners: Box<[EventTransactionProducerOwner]>,
    assertions: Box<[SolveEventAction]>,
    assertion_action_indices: Box<[Box<[usize]>]>,
    statement_count: usize,
    clock_owners: Box<[PeriodicClockId]>,
    span: Span,
}

/// Uncommitted inputs to the checked event-transaction constructor.
///
/// This carrier may be assembled by a phase or wire adapter, but it cannot be
/// used as executable Solve IR until [`EventTransactionProgram::checked`]
/// validates and owns every column.
pub struct EventTransactionConstruction {
    pub site: SolvePureCallSite,
    pub inputs: Vec<(ScalarSlot, SolveValueType)>,
    pub targets: Vec<(ScalarSlot, SolveValueType, Option<PeriodicClockId>)>,
    pub producer_owners: Vec<EventTransactionProducerOwner>,
    pub assertions: Vec<SolveEventAction>,
    pub assertion_action_indices: Vec<Vec<usize>>,
    pub statement_count: usize,
    pub clock_owners: Vec<PeriodicClockId>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EventTransactionProgramWire {
    site: SolvePureCallSite,
    inputs: Vec<EventTransactionInputWire>,
    targets: Vec<EventTransactionTargetWire>,
    producer_owners: Vec<EventTransactionProducerOwner>,
    assertions: Vec<SolveEventAction>,
    assertion_action_indices: Vec<Vec<usize>>,
    statement_count: usize,
    clock_owners: Vec<PeriodicClockId>,
    span: Span,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EventTransactionInputWire {
    source: ScalarSlot,
    value_type: SolveValueType,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EventTransactionTargetWire {
    base: ScalarSlot,
    value_type: SolveValueType,
    #[serde(deserialize_with = "deserialize_required_option")]
    clock_owner: Option<PeriodicClockId>,
}

impl<'de> Deserialize<'de> for EventTransactionProgram {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = EventTransactionProgramWire::deserialize(deserializer)?;
        let provenance = wire
            .span
            .require_provenance("EventTransactionProgram")
            .map_err(serde::de::Error::custom)?;
        Self::checked(
            EventTransactionConstruction {
                site: wire.site,
                inputs: wire
                    .inputs
                    .into_iter()
                    .map(|input| (input.source, input.value_type))
                    .collect(),
                targets: wire
                    .targets
                    .into_iter()
                    .map(|target| (target.base, target.value_type, target.clock_owner))
                    .collect(),
                producer_owners: wire.producer_owners,
                assertions: wire.assertions,
                assertion_action_indices: wire.assertion_action_indices,
                statement_count: wire.statement_count,
                clock_owners: wire.clock_owners,
            },
            provenance,
        )
        .map_err(serde::de::Error::custom)
    }
}

impl EventTransactionProgram {
    pub fn checked(
        construction: EventTransactionConstruction,
        provenance: ProvenanceSpan,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let span = provenance.span();
        let inputs = construction
            .inputs
            .into_iter()
            .map(|(source, value_type)| EventTransactionInput { source, value_type })
            .collect::<Box<[_]>>();
        let targets = construction
            .targets
            .into_iter()
            .map(|(base, value_type, clock_owner)| EventTransactionTarget {
                base,
                value_type,
                clock_owner,
            })
            .collect::<Box<[_]>>();
        let producer_owners = construction.producer_owners.into_boxed_slice();
        let assertions = construction.assertions.into_boxed_slice();
        let assertion_action_indices = construction
            .assertion_action_indices
            .into_iter()
            .map(Vec::into_boxed_slice)
            .collect::<Box<[_]>>();
        let program = Self {
            site: construction.site,
            inputs,
            targets,
            producer_owners,
            assertions,
            assertion_action_indices,
            statement_count: construction.statement_count,
            clock_owners: construction.clock_owners.into_boxed_slice(),
            span,
        };
        validate_event_transaction_interface(&program)?;
        Ok(program)
    }

    #[must_use]
    pub const fn site(&self) -> &SolvePureCallSite {
        &self.site
    }

    #[must_use]
    pub const fn inputs(&self) -> &[EventTransactionInput] {
        &self.inputs
    }

    #[must_use]
    pub const fn targets(&self) -> &[EventTransactionTarget] {
        &self.targets
    }

    /// Row-aligned compiler-issued projection to the B.1b/B.1c producer owner.
    #[must_use]
    pub const fn producer_owners(&self) -> &[EventTransactionProducerOwner] {
        &self.producer_owners
    }

    /// Ordered actions aligned with the assertion-predicate result suffix.
    #[must_use]
    pub const fn assertions(&self) -> &[SolveEventAction] {
        &self.assertions
    }

    /// Exact event-action output replaced by each predicate suffix lane.
    #[must_use]
    pub const fn assertion_action_indices(&self) -> &[Box<[usize]>] {
        &self.assertion_action_indices
    }

    /// Number of call-scoped assertion predicates returned atomically after
    /// the target tuple.
    #[must_use]
    pub fn assertion_count(&self) -> usize {
        self.assertions.len()
    }

    #[must_use]
    pub const fn statement_count(&self) -> usize {
        self.statement_count
    }

    #[must_use]
    pub const fn clock_owners(&self) -> &[PeriodicClockId] {
        &self.clock_owners
    }

    #[must_use]
    pub const fn is_clock_owned(&self) -> bool {
        !self.clock_owners.is_empty()
    }

    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }
}

fn validate_event_transaction_interface(
    program: &EventTransactionProgram,
) -> Result<(), SolveProblemShapeContractError> {
    let EventTransactionProgram {
        site,
        inputs,
        targets,
        producer_owners,
        assertions,
        assertion_action_indices,
        statement_count,
        clock_owners,
        span,
    } = program;
    let invalid = |detail| SolveProblemShapeContractError::EventTransactionProgram {
        program_index: 0,
        detail,
        span: Some(*span),
    };
    if *statement_count == 0 || targets.is_empty() {
        return Err(invalid("statement or target catalog is empty"));
    }
    validate_event_transaction_clocks(targets, clock_owners, &invalid)?;
    if producer_owners.len() != targets.len() {
        return Err(invalid(
            "producer projections do not cover the complete target tuple",
        ));
    }
    validate_event_transaction_call(site, inputs, targets, assertions, &invalid)?;
    validate_event_transaction_assertions(
        assertions,
        assertion_action_indices,
        clock_owners,
        &invalid,
    )?;
    for input in inputs {
        validate_event_transaction_source(input, &invalid)?;
    }
    validate_event_transaction_targets(targets, &invalid)
}

fn validate_event_transaction_clocks(
    targets: &[EventTransactionTarget],
    clock_owners: &[PeriodicClockId],
    invalid: &impl Fn(&'static str) -> SolveProblemShapeContractError,
) -> Result<(), SolveProblemShapeContractError> {
    if clock_owners
        .windows(2)
        .any(|pair| pair[0].index() >= pair[1].index())
    {
        return Err(invalid("activation clocks are not sorted and unique"));
    }
    let target_clocks = targets
        .iter()
        .filter_map(EventTransactionTarget::clock_owner)
        .collect::<std::collections::BTreeSet<_>>();
    let clock_owned = !clock_owners.is_empty();
    if targets
        .iter()
        .any(|target| target.clock_owner().is_some() != clock_owned)
        || target_clocks
            .iter()
            .copied()
            .ne(clock_owners.iter().copied())
    {
        return Err(invalid(
            "target clocks do not exactly cover the canonical activation-clock set",
        ));
    }
    Ok(())
}

fn validate_event_transaction_call(
    site: &SolvePureCallSite,
    inputs: &[EventTransactionInput],
    targets: &[EventTransactionTarget],
    assertions: &[SolveEventAction],
    invalid: &impl Fn(&'static str) -> SolveProblemShapeContractError,
) -> Result<(), SolveProblemShapeContractError> {
    if site.inputs().len() != inputs.len()
        || site
            .inputs()
            .iter()
            .zip(inputs)
            .any(|(expected, input)| expected != &input.value_type)
    {
        return Err(invalid("typed call inputs do not match storage inputs"));
    }
    if site.outputs().len() < targets.len()
        || site.outputs()[..targets.len()]
            .iter()
            .zip(targets)
            .any(|(output, target)| {
                output.kind() != SolvePureCallOutputKind::Result
                    || output.value_type() != &target.value_type
            })
    {
        return Err(invalid("typed call outputs do not match atomic targets"));
    }
    if site.outputs()[targets.len()..].iter().any(|output| {
        output.kind() != SolvePureCallOutputKind::AssertionPredicate
            || output.value_type() != &SolveValueType::scalar(SolveScalarType::Boolean)
    }) {
        return Err(invalid(
            "transaction suffix is not a checked assertion-predicate tuple",
        ));
    }
    if site.outputs().len() - targets.len() != assertions.len() {
        return Err(invalid(
            "assertion actions do not cover the checked predicate suffix",
        ));
    }
    Ok(())
}

fn validate_event_transaction_assertions(
    assertions: &[SolveEventAction],
    assertion_action_indices: &[Box<[usize]>],
    clock_owners: &[PeriodicClockId],
    invalid: &impl Fn(&'static str) -> SolveProblemShapeContractError,
) -> Result<(), SolveProblemShapeContractError> {
    if assertion_action_indices.len() != assertions.len() {
        return Err(invalid(
            "event-action projections do not cover the predicate suffix",
        ));
    }
    if assertion_action_indices
        .iter()
        .any(|indices| indices.is_empty())
    {
        return Err(invalid(
            "one assertion predicate has no event-action projection",
        ));
    }
    let clock_owned = !clock_owners.is_empty();
    if assertions.iter().any(|action| {
        !matches!(action.kind, SolveEventActionKind::Assert)
            || action.clock_owner.is_some() != clock_owned
            || action
                .clock_owner
                .is_some_and(|clock| clock_owners.binary_search(&clock).is_err())
            || action.span.is_dummy()
    }) {
        return Err(invalid(
            "assertion action kind, clock owner, or provenance is invalid",
        ));
    }
    Ok(())
}

fn validate_event_transaction_source(
    input: &EventTransactionInput,
    invalid: &impl Fn(&'static str) -> SolveProblemShapeContractError,
) -> Result<(), SolveProblemShapeContractError> {
    match input.source {
        ScalarSlot::Y { index, .. } | ScalarSlot::P { index, .. } => {
            index
                .checked_add(input.value_type.scalar_count() as usize)
                .ok_or_else(|| invalid("input storage range overflows"))?;
        }
        ScalarSlot::Time | ScalarSlot::Constant(_) => {
            if input.value_type.scalar_count() != 1 {
                return Err(invalid("time or constant input is not scalar"));
            }
        }
    }
    Ok(())
}

fn validate_event_transaction_targets(
    targets: &[EventTransactionTarget],
    invalid: &impl Fn(&'static str) -> SolveProblemShapeContractError,
) -> Result<(), SolveProblemShapeContractError> {
    let mut ranges = Vec::with_capacity(targets.len());
    for target in targets {
        let (storage, start) = match target.base {
            ScalarSlot::Y { index, .. } => (0_u8, index),
            ScalarSlot::P { index, .. } => (1_u8, index),
            ScalarSlot::Time | ScalarSlot::Constant(_) => {
                return Err(invalid("target is not mutable Y/P storage"));
            }
        };
        let end = start
            .checked_add(target.value_type.scalar_count() as usize)
            .ok_or_else(|| invalid("target storage range overflows"))?;
        if ranges
            .iter()
            .any(|&(other_storage, other_start, other_end)| {
                storage == other_storage && start < other_end && other_start < end
            })
        {
            return Err(invalid("target storage ranges overlap"));
        }
        ranges.push((storage, start, end));
    }
    Ok(())
}
