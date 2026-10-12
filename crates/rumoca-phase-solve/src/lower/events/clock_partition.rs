//! Solve lowering of the SPEC_0040 SOLVE-C57 clock-partition same-tick order.
//!
//! Two decisions live here and nowhere else: which clock-owned producers may
//! share one fused program (a fused program is a single entry read, so only
//! producers that cannot observe one another on the tick may share it), and
//! the issued execution order over the producers that *do* observe one
//! another. Both are values decided at construction; the runtime replays them.

use super::*;

use crate::lower::clocks::clocks_share_a_tick;

impl From<rumoca_phase_structural::SameTickOrderError> for LowerError {
    fn from(error: rumoca_phase_structural::SameTickOrderError) -> Self {
        use rumoca_phase_structural::SameTickOrderError;
        match &error {
            SameTickOrderError::Cycle { .. } | SameTickOrderError::UnrefreshableRead { .. } => {
                Self::non_computable(error.to_string(), error.span())
            }
            SameTickOrderError::DuplicateOwner { .. }
            | SameTickOrderError::InvalidClockDomains { .. }
            | SameTickOrderError::MissingDerivative { .. }
            | SameTickOrderError::UnsupportedDerivativeValue { .. }
            | SameTickOrderError::Projection(_) => Self::contract(error.to_string(), error.span()),
        }
    }
}

/// Issue the SOLVE-C57 clock-partition same-tick schedule.
///
/// The structural engine (`rumoca_phase_structural::issue_same_tick_schedule`)
/// orders the recorded clock-owned producers by their same-instant reads,
/// alias-followed through the exact definitions of `SameTickDefinitions`, and
/// names the intermediates that sit on producer-to-producer paths. This
/// function replays that issued schedule into the Solve IR: producer steps by
/// typed index, intermediate-definition refresh rows lowered exactly once
/// each. Equation-shaped projections owned by a DAE-C21/SOLVE-C55 event
/// transaction are excluded; the complete opaque transaction replaces them as
/// one producer in this same issued order. A same-tick cycle with no `pre()`/`previous()`
/// boundary is rejected here, at construction, at the blocked producer's span.
pub(super) fn issue_clock_partition_order<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    clocks: &LoweredClocks<'dae>,
    discrete: &mut DiscreteRows<'dae>,
) -> Result<(), LowerError> {
    let pending = std::mem::take(&mut discrete.clocked_producers);
    let mut excluded = BTreeSet::new();
    let mut admitted = Vec::new();
    for producer in &pending {
        if matches!(producer.step, PendingClockedStep::EventTransaction { .. }) {
            admitted.push(producer);
        } else if discrete.producer_owned_by_transaction(producer) {
            excluded.extend(producer.targets.iter().map(|target| target.index()));
        } else {
            admitted.push(producer);
        }
    }
    if admitted.is_empty() {
        return Ok(());
    }
    let producers = admitted
        .iter()
        .map(|producer| rumoca_phase_structural::SameTickProducer {
            read_context: producer.read_context.clone(),
            clock_domains: producer
                .clock_owners
                .iter()
                .map(|clock| u32::try_from(clock.index()).expect("Solve clock identity fits u32"))
                .collect(),
            targets: producer.targets.clone(),
            value_reads: producer.value_reads.clone(),
            condition_reads: producer.condition_reads.clone(),
            span: producer.span,
        })
        .collect::<Vec<_>>();
    let coincidence = producer_clock_coincidence(&clocks.partition, &producers);
    let schedule_result = rumoca_phase_structural::issue_same_tick_schedule(
        view,
        &discrete.same_tick_definitions,
        &producers,
        &excluded,
        &coincidence,
    );
    let schedule = schedule_result?;
    let mut order = Vec::with_capacity(schedule.steps.len());
    let intermediate_consumers = schedule.intermediate_consumers;
    for step in schedule.steps {
        match step {
            rumoca_phase_structural::SameTickStep::Producer(index) => {
                order.push(match admitted[index].step {
                    PendingClockedStep::ScalarRows { start_row, count } => {
                        solve::ClockPartitionStep::ScalarRows { start_row, count }
                    }
                    PendingClockedStep::GuardedAssignment { program_index } => {
                        solve::ClockPartitionStep::GuardedAssignment { program_index }
                    }
                    PendingClockedStep::StructuredUpdate { update_index } => {
                        solve::ClockPartitionStep::StructuredUpdate { update_index }
                    }
                    PendingClockedStep::EventTransaction { program_index } => {
                        solve::ClockPartitionStep::EventTransaction { program_index }
                    }
                });
            }
            rumoca_phase_structural::SameTickStep::IntermediateDefinition {
                identity,
                variable,
                definition,
                read_boundary,
            } => {
                let Some(consumers) = intermediate_consumers.get(&identity) else {
                    return Err(LowerError::contract(
                        "issued clock-partition intermediate has no consumer set",
                        intermediate_variable_span(view, variable),
                    ));
                };
                issue_intermediate_definition(
                    view,
                    layout,
                    discrete,
                    &admitted,
                    IntermediateDefinitionIssue {
                        variable,
                        definition,
                        consumers,
                        read_boundary,
                    },
                    &mut order,
                )?;
            }
        }
    }
    discrete.clock_partition_order = order;
    Ok(())
}

struct IntermediateDefinitionIssue<'a, 'dae> {
    variable: dae::VariableId<'dae>,
    definition: dae::ExprId<'dae>,
    consumers: &'a [usize],
    read_boundary: rumoca_phase_structural::SameTickReadBoundary,
}

fn issue_intermediate_definition<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    discrete: &mut DiscreteRows<'dae>,
    admitted: &[&PendingClockedProducer<'dae>],
    issue: IntermediateDefinitionIssue<'_, 'dae>,
    order: &mut Vec<solve::ClockPartitionStep>,
) -> Result<(), LowerError> {
    let span = intermediate_variable_span(view, issue.variable);
    let mut consumer_clocks = issue
        .consumers
        .iter()
        .flat_map(|&consumer| {
            admitted
                .get(consumer)
                .map(|producer| {
                    producer
                        .clock_owners
                        .iter()
                        .copied()
                        .map(Ok)
                        .collect::<Vec<_>>()
                })
                .unwrap_or_else(|| {
                    vec![Err(LowerError::contract(
                        "issued intermediate consumer is out of bounds",
                        span,
                    ))]
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    consumer_clocks.sort_by_key(|clock| clock.index());
    consumer_clocks.dedup();
    if consumer_clocks.is_empty() {
        return Err(LowerError::contract(
            "issued clock-partition intermediate has no consumer clock",
            span,
        ));
    }
    let definition_span = view
        .expression(issue.definition)
        .expect("checked issued definition resolves")
        .provenance()
        .span();
    let scalar_count = view
        .variable(issue.variable)
        .expect("checked intermediate variable resolves")
        .scalar_count();
    for scalar in 0..scalar_count {
        let compiler = layout.scalar_compiler(view, None);
        let program =
            if issue.read_boundary == rumoca_phase_structural::SameTickReadBoundary::Current {
                compiler.current_definition_program(
                    crate::lower::scalar::DefinitionOwner::new(
                        std::rc::Rc::clone(&discrete.same_tick_definitions),
                        std::rc::Rc::new(
                            admitted
                                .iter()
                                .flat_map(|producer| {
                                    producer.targets.iter().map(|target| target.index())
                                })
                                .collect(),
                        ),
                    ),
                    issue.definition,
                    scalar,
                )?
            } else {
                compiler.program(issue.definition, scalar)?
            };
        let target = variable_scalar_slot(layout, issue.variable.index(), scalar, definition_span)?;
        let row = discrete.clock_partition_intermediate_targets.len();
        discrete
            .clock_partition_intermediates
            .push(program, definition_span, row);
        discrete.clock_partition_intermediate_targets.push(target);
        discrete
            .clock_partition_intermediate_clocks
            .push(consumer_clocks.clone());
        order.push(solve::ClockPartitionStep::Intermediate { row });
    }
    Ok(())
}

/// The SOLVE-C57 exchange relation over the producers' periodic clocks.
///
/// Two distinct clock identities exchange same-tick values exactly when the
/// rational clock lattice proves they can fire on one shared instant
/// ([`clocks_share_a_tick`]: same anchor, phase difference an integer multiple
/// of the gcd of the periods); identical identities need no proof. The
/// relation is pairwise, never closed into classes, because coincidence is
/// not transitive. Clocks that never coincide keep the held entry-storage
/// read, the SOLVE-C22 left-limit lane, exactly as before.
fn producer_clock_coincidence(
    partition: &solve::SolveClockPartition,
    producers: &[rumoca_phase_structural::SameTickProducer<'_>],
) -> rumoca_phase_structural::ClockCoincidence {
    let domains: Vec<u32> = producers
        .iter()
        .flat_map(|producer| producer.clock_domains.iter().copied())
        .collect::<BTreeSet<u32>>()
        .into_iter()
        .collect();
    let mut pairs = Vec::new();
    for (position, &left) in domains.iter().enumerate() {
        for &right in &domains[position + 1..] {
            if clocks_share_a_tick(partition, left as usize, right as usize) {
                pairs.push((left, right));
            }
        }
    }
    rumoca_phase_structural::ClockCoincidence::of_pairs(pairs)
}

fn intermediate_variable_span<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableId<'dae>,
) -> Span {
    view.variable(variable)
        .expect("checked intermediate variable resolves")
        .declaration()
        .span()
}

/// One clock-owned producer awaiting its issued same-tick rank.
///
/// `value_reads`/`condition_reads` are the roots the producer evaluates at its
/// tick; a producer whose reads resolve through a history lane (a `sample(u)`
/// source) records none, which is exactly the SOLVE-C28 boundary.
pub(super) struct PendingClockedProducer<'dae> {
    pub(super) read_context: rumoca_phase_structural::SameTickReadContext<'dae>,
    pub(super) step: PendingClockedStep,
    pub(super) clock_owners: Vec<solve::PeriodicClockId>,
    pub(super) targets: Vec<dae::VariableId<'dae>>,
    pub(super) value_reads: Vec<dae::ExprId<'dae>>,
    pub(super) condition_reads: Vec<dae::ConditionId<'dae>>,
    pub(super) span: Span,
}

#[derive(Clone, Copy)]
pub(super) enum PendingClockedStep {
    ScalarRows { start_row: usize, count: usize },
    GuardedAssignment { program_index: usize },
    StructuredUpdate { update_index: usize },
    EventTransaction { program_index: usize },
}

/// The value and condition roots one guarded owner group evaluates at its tick,
/// as SOLVE-C57 same-tick read roots: every arm's value, trigger, and guard.
pub(super) fn guarded_group_same_tick_reads<'dae>(
    group: &[GuardedTarget<'dae>],
) -> (Vec<dae::ExprId<'dae>>, Vec<dae::ConditionId<'dae>>) {
    let mut value_reads = Vec::new();
    let mut condition_reads = Vec::new();
    for &(trigger, guard, value, _) in group.iter().flat_map(|target| &target.branches) {
        value_reads.push(value);
        condition_reads.push(trigger);
        condition_reads.push(guard);
    }
    (value_reads, condition_reads)
}

/// One prospective producer in a fusion candidate set: what it writes, and the
/// alias-closed set of coordinates it observes at its tick.
pub(super) struct SameTickExchangeMember<'dae> {
    pub(super) event_reads: Option<rumoca_phase_structural::EventReadFacts<'dae>>,
    pub(super) targets: Vec<u32>,
    pub(super) reads: BTreeSet<u32>,
    /// Coordinates this member reads at the event-iteration entry: the
    /// source trigger conditions of an unclocked `when`. Their producers
    /// are issued after this member, so it never observes an update of the
    /// same iteration.
    pub(super) entry_reads: BTreeSet<u32>,
}

/// The same-tick observation relation over one fusion candidate set.
///
/// A fused program performs exactly one entry read, so two targets may share
/// one program only when neither observes the other on the tick — directly or
/// through any chain inside the set. Everything the members *do* observe is
/// sequenced by the SOLVE-C57 issued order instead, which is why restoring
/// fusion cannot reintroduce a stale same-tick read.
pub(super) struct SameTickExchange<'dae> {
    event_reads: Vec<Option<rumoca_phase_structural::EventReadFacts<'dae>>>,
    direct: Vec<BTreeSet<usize>>,
    observes: Vec<BTreeSet<usize>>,
    /// For each member, the members that read one of its targets at the
    /// event-iteration entry and so are issued before it.
    entry_readers: Vec<BTreeSet<usize>>,
}

impl<'dae> SameTickExchange<'dae> {
    pub(super) fn new(members: &[SameTickExchangeMember<'dae>]) -> Self {
        let mut owner_of = BTreeMap::<u32, usize>::new();
        for (index, member) in members.iter().enumerate() {
            for &target in &member.targets {
                owner_of.entry(target).or_insert(index);
            }
        }
        let owners = |index: usize, reads: &BTreeSet<u32>| {
            reads
                .iter()
                .filter_map(|read| owner_of.get(read).copied())
                .filter(move |&owner| owner != index)
                .collect::<BTreeSet<_>>()
        };
        let direct = members
            .iter()
            .enumerate()
            .map(|(index, member)| owners(index, &member.reads))
            .collect::<Vec<_>>();
        let mut observes = vec![BTreeSet::new(); members.len()];
        for (start, observation) in observes.iter_mut().enumerate() {
            *observation = transitive_observations(start, &direct);
        }
        let mut entry_readers = vec![BTreeSet::new(); members.len()];
        for (reader, member) in members.iter().enumerate() {
            for producer in owners(reader, &member.entry_reads) {
                entry_readers[producer].insert(reader);
            }
        }
        Self {
            event_reads: members
                .iter()
                .map(|member| member.event_reads.clone())
                .collect(),
            direct,
            observes,
            entry_readers,
        }
    }

    pub(super) fn derive(
        view: dae::DaeView<'dae>,
        layout: &EventCompilationContext<'_, 'dae>,
        clocks: &LoweredClocks<'dae>,
        definitions: &rumoca_phase_structural::SameTickDefinitions<'dae>,
        targets: &[GuardedTarget<'dae>],
    ) -> Result<Self, LowerError> {
        let event_clocked = targets
            .iter()
            .filter(|target| target.event_clock.is_some())
            .map(|target| target.variable.index())
            .collect::<BTreeSet<_>>();
        let members = targets
            .iter()
            .map(|target| {
                if is_ordinary_target(clocks, target.variable) {
                    return unclocked_member(view, layout, definitions, target);
                }
                let (value_reads, condition_reads) =
                    guarded_group_same_tick_reads(std::slice::from_ref(target));
                if target.event_clock.is_some() {
                    return event_clock_member(
                        view,
                        definitions,
                        target,
                        (&value_reads, &condition_reads),
                        |variable| event_clocked.contains(&variable),
                    );
                }
                Ok(SameTickExchangeMember {
                    event_reads: None,
                    targets: vec![target.variable.index()],
                    reads: definitions.clock_partition_read_closure(
                        view,
                        &value_reads,
                        &condition_reads,
                    )?,
                    entry_reads: BTreeSet::new(),
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        Ok(Self::new(&members))
    }

    pub(super) fn group_read_context(
        &self,
        order: &[usize],
        first: usize,
        end: usize,
    ) -> rumoca_phase_structural::SameTickReadContext<'dae> {
        let facts = order[first..end]
            .iter()
            .map(|&index| self.event_reads[index].as_ref())
            .collect::<Option<Vec<_>>>();
        match facts {
            Some(facts) if !facts.is_empty() => {
                rumoca_phase_structural::SameTickReadContext::OrdinaryEvent(
                    rumoca_phase_structural::EventReadFacts::merge(facts[0], &facts[1..]),
                )
            }
            Some(_) | None => rumoca_phase_structural::SameTickReadContext::ClockPartition,
        }
    }

    fn fusable(&self, left: usize, right: usize) -> bool {
        !self.observes[left].contains(&right) && !self.observes[right].contains(&left)
    }

    /// Whether `candidate` may join the family already spanning `first..candidate`.
    pub(super) fn fusable_with_range(
        &self,
        order: &[usize],
        first: usize,
        candidate: usize,
    ) -> bool {
        (first..candidate).all(|member| self.fusable(order[member], order[candidate]))
    }

    /// A stable producer-before-reader order of the members: each member is
    /// placed after every member it observes that does not in turn observe it,
    /// and after every member that reads one of its targets at the iteration
    /// entry. Members of a cycle keep their relative input order.
    pub(super) fn producer_order(&self) -> Vec<usize> {
        let count = self.observes.len();
        let mut placed = vec![false; count];
        let mut order = Vec::with_capacity(count);
        // When no member is ready, the unplaced members wait on an observation
        // cycle, and the first unplaced member breaks it.
        while let Some(next) = (0..count)
            .find(|&member| !placed[member] && self.producers_placed(member, &placed))
            .or_else(|| (0..count).find(|&member| !placed[member]))
        {
            placed[next] = true;
            order.push(next);
        }
        order
    }

    /// Whether every member `member` reads directly is placed or observes it back,
    /// and every entry reader of its targets is placed.
    fn producers_placed(&self, member: usize, placed: &[bool]) -> bool {
        self.direct[member]
            .iter()
            .all(|&observed| placed[observed] || self.observes[observed].contains(&member))
            && self.entry_readers[member]
                .iter()
                .all(|&reader| placed[reader])
    }

    /// Whether every member of the set may share one program.
    pub(super) fn all_fusable(&self) -> bool {
        self.observes.iter().all(BTreeSet::is_empty)
    }
}

/// Source activations read entry values; when-body conditions read the active
/// equations' same-instant values (MLS §8.5 / SOLVE-C58).
fn unclocked_member<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    definitions: &rumoca_phase_structural::SameTickDefinitions<'dae>,
    target: &GuardedTarget<'dae>,
) -> Result<SameTickExchangeMember<'dae>, LowerError> {
    let triggers = target
        .branches
        .iter()
        .map(|branch| branch.0)
        .collect::<Vec<_>>();
    let mut collector = definitions.event_read_collector(view);
    for &(trigger, guard, value, _) in &target.branches {
        collector.expression(value)?;
        collector.condition(guard, &BTreeSet::from([trigger]))?;
    }
    for trigger in triggers {
        collector.entry_condition(trigger, |expression| {
            layout
                .buffered_relations
                .expression_slot(expression)
                .is_some()
        })?;
    }
    let facts = collector.finish()?;
    Ok(SameTickExchangeMember {
        targets: vec![target.variable.index()],
        reads: facts.current().clone(),
        entry_reads: facts.entry().clone(),
        event_reads: Some(facts),
    })
}

/// The same-tick relation of one target of an MLS §16.3 event clock.
///
/// The partition's values, and the conditions of `if` branches in its `when`
/// bodies, are values of the tick: the target observes every event-clock
/// target it reads, of its own clock or, through an MLS §16.5.2 `shiftSample`,
/// of the clock it is shifted from. Any other coordinate reaches the partition
/// only through `sample(u)` at its left limit (MLS §16.5.1) or as the tick
/// condition (SOLVE-C58), so it and everything its definition reads is an entry
/// read, which issues the target before those coordinates' producers.
fn event_clock_member<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::SameTickDefinitions<'dae>,
    target: &GuardedTarget<'dae>,
    (value_reads, condition_reads): (&[dae::ExprId<'dae>], &[dae::ConditionId<'dae>]),
    event_clocked: impl Fn(u32) -> bool,
) -> Result<SameTickExchangeMember<'dae>, LowerError> {
    let mut direct = BTreeSet::new();
    for &value in value_reads {
        rumoca_phase_structural::collect_same_instant_reads(view, value, &mut direct);
    }
    for &condition in condition_reads {
        rumoca_phase_structural::collect_condition_same_instant_reads(view, condition, &mut direct);
    }
    let (reads, sampled): (BTreeSet<u32>, BTreeSet<u32>) = direct
        .into_iter()
        .partition(|&variable| event_clocked(variable));
    Ok(SameTickExchangeMember {
        event_reads: None,
        targets: vec![target.variable.index()],
        reads,
        entry_reads: definitions.clock_partition_variable_read_closure(view, sampled)?,
    })
}

fn transitive_observations(start: usize, direct: &[BTreeSet<usize>]) -> BTreeSet<usize> {
    let mut observations = BTreeSet::new();
    let mut stack = direct[start].iter().copied().collect::<Vec<_>>();
    while let Some(node) = stack.pop() {
        if observations.insert(node) {
            stack.extend(direct[node].iter().copied());
        }
    }
    observations
}

pub(super) fn guarded_definition_owner<'dae>(
    rows: &DiscreteRows<'dae>,
    targets: &[GuardedTarget<'dae>],
) -> crate::lower::scalar::DefinitionOwner<'dae> {
    let definition_targets = std::rc::Rc::new(
        targets
            .iter()
            .map(|target| target.variable.index())
            .chain(
                rows.clocked_producers
                    .iter()
                    .flat_map(|producer| producer.targets.iter().map(|target| target.index())),
            )
            .collect::<BTreeSet<_>>(),
    );
    crate::lower::scalar::DefinitionOwner::new(
        std::rc::Rc::clone(&rows.same_tick_definitions),
        definition_targets,
    )
}

#[cfg(test)]
mod event_order_tests;
