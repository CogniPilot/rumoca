//! Solve lowering of the SPEC_0040 SOLVE-C57 clock-partition same-tick order.
//!
//! Two decisions live here and nowhere else: which clock-owned producers may
//! share one fused program (a fused program is a single entry read, so only
//! producers that cannot observe one another on the tick may share it), and
//! the issued execution order over the producers that *do* observe one
//! another. Both are values decided at construction; the runtime replays them.

use super::*;

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
    layout: &LoweredLayout<'dae>,
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
    let schedule_result = rumoca_phase_structural::issue_same_tick_schedule(
        view,
        &discrete.same_tick_definitions,
        &producers,
        &excluded,
    );
    let schedule = schedule_result.map_err(|error| match &error {
        rumoca_phase_structural::SameTickOrderError::Cycle { .. }
        | rumoca_phase_structural::SameTickOrderError::UnrefreshableRead { .. } => {
            LowerError::non_computable(error.to_string(), error.span())
        }
        rumoca_phase_structural::SameTickOrderError::DuplicateOwner { .. }
        | rumoca_phase_structural::SameTickOrderError::InvalidClockDomains { .. } => {
            LowerError::contract(error.to_string(), error.span())
        }
    })?;
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
                variable,
                definition,
            } => {
                let Some(consumers) = intermediate_consumers.get(&variable.index()) else {
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
}

fn issue_intermediate_definition<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
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
        let program = ScalarCompiler::new(view, layout, None).program(issue.definition, scalar)?;
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
pub(super) struct SameTickExchangeMember {
    pub(super) targets: Vec<u32>,
    pub(super) reads: BTreeSet<u32>,
}

/// The same-tick observation relation over one fusion candidate set.
///
/// A fused program performs exactly one entry read, so two targets may share
/// one program only when neither observes the other on the tick — directly or
/// through any chain inside the set. Everything the members *do* observe is
/// sequenced by the SOLVE-C57 issued order instead, which is why restoring
/// fusion cannot reintroduce a stale same-tick read.
pub(super) struct SameTickExchange {
    observes: Vec<BTreeSet<usize>>,
}

impl SameTickExchange {
    pub(super) fn new(members: &[SameTickExchangeMember]) -> Self {
        let mut owner_of = BTreeMap::<u32, usize>::new();
        for (index, member) in members.iter().enumerate() {
            for &target in &member.targets {
                owner_of.entry(target).or_insert(index);
            }
        }
        let direct = members
            .iter()
            .enumerate()
            .map(|(index, member)| {
                member
                    .reads
                    .iter()
                    .filter_map(|read| owner_of.get(read).copied())
                    .filter(|&owner| owner != index)
                    .collect::<BTreeSet<_>>()
            })
            .collect::<Vec<_>>();
        let mut observes = vec![BTreeSet::new(); members.len()];
        for (start, observation) in observes.iter_mut().enumerate() {
            *observation = transitive_observations(start, &direct);
        }
        Self { observes }
    }

    pub(super) fn derive<'dae>(
        view: dae::DaeView<'dae>,
        definitions: &rumoca_phase_structural::SameTickDefinitions<'dae>,
        targets: &[GuardedTarget<'dae>],
    ) -> Self {
        let members = targets
            .iter()
            .map(|target| {
                let (value_reads, condition_reads) =
                    guarded_group_same_tick_reads(std::slice::from_ref(target));
                SameTickExchangeMember {
                    targets: vec![target.variable.index()],
                    reads: definitions.read_closure(view, &value_reads, &condition_reads),
                }
            })
            .collect::<Vec<_>>();
        Self::new(&members)
    }

    fn fusable(&self, left: usize, right: usize) -> bool {
        !self.observes[left].contains(&right) && !self.observes[right].contains(&left)
    }

    /// Whether `candidate` may join the family already spanning `first..candidate`.
    pub(super) fn fusable_with_range(&self, first: usize, candidate: usize) -> bool {
        (first..candidate).all(|member| self.fusable(member, candidate))
    }

    /// Whether every member of the set may share one program.
    pub(super) fn all_fusable(&self) -> bool {
        self.observes.iter().all(BTreeSet::is_empty)
    }
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
