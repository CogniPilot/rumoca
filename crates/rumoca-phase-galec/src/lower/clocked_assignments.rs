//! Checked B.1c and clocked discrete-Real projection into GALEC `DoStep`.

#[cfg(test)]
mod tests;

use std::collections::HashSet;

use super::*;

#[derive(Clone)]
pub(super) struct ClockedAssignment {
    pub(super) targets: HashSet<u32>,
    pub(super) reads: HashSet<u32>,
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) span: Span,
    pub(super) is_preamble: bool,
    pub(super) requires_preamble: bool,
}

pub(super) struct ClockedAssignments {
    #[cfg(test)]
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) locals: Vec<gast::VariableDeclaration>,
    pub(super) called_user_functions: HashSet<u32>,
    pub(super) assignments: Vec<ClockedAssignment>,
    /// Whether this domain evaluates a shared call at a scheduled position.
    pub(super) schedules_shared_calls: bool,
}

/// The activation a clock-domain value is evaluated under.
///
/// `Entry` is a value the domain clock alone selects: `Always`, or a `when`
/// whose guard names nothing but that clock. Everything else carries the
/// runtime guard it ran under, and may only be shared with values carrying the
/// identical one.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum ValueActivation<'dae> {
    Entry,
    Guarded {
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
    },
}

/// One clock domain's shared eager-call materialization.
struct SharedClockCalls<'dae> {
    /// Result temporaries a consumer selected by the domain clock alone may
    /// restore: the domain-entry preamble's and the unguarded scheduled
    /// node's.
    entry: SharedMaterializedFunctionCalls,
    /// Result temporaries per guarded activation, readable only by a consumer
    /// carrying that identical activation.
    guarded: HashMap<ValueActivation<'dae>, SharedMaterializedFunctionCalls>,
    /// The schedule node each shared call owner was materialized by.
    owners: HashMap<u32, u32>,
    /// Whether any scheduled node was emitted at all.
    scheduled: bool,
}

impl<'dae> SharedClockCalls<'dae> {
    /// The result temporaries a group running under `activation` may restore.
    ///
    /// A guarded node's temporaries are written under its own guard, so only a
    /// group carrying that identical activation is proven to run where they
    /// hold. Every other group sees the entry set alone.
    fn calls_for(&self, activation: ValueActivation<'dae>) -> &SharedMaterializedFunctionCalls {
        self.guarded.get(&activation).unwrap_or(&self.entry)
    }

    /// The schedule edges a group that consumed `consumed` has to declare.
    fn consumed_reads<'a>(&'a self, consumed: &'a HashSet<u32>) -> impl Iterator<Item = u32> + 'a {
        consumed
            .iter()
            .filter_map(|owner| self.owners.get(owner).copied())
    }
}

#[cfg(test)]
pub(super) fn lower_clocked_assignments<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
) -> Result<ClockedAssignments, GalecTargetError> {
    lower_clocked_assignments_for_domain(view, definitions, clock, by_id, pre_names, true, true)
}

pub(super) fn lower_clocked_assignments_for_domain<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    include_unclocked_actions: bool,
    allow_scheduled_shared_calls: bool,
) -> Result<ClockedAssignments, GalecTargetError> {
    let mut pending = Vec::new();
    let mut locals = Vec::new();
    let mut called_user_functions = HashSet::new();
    let mut lowerer = ExpressionLowerer::with_do_step_effects(view, definitions, by_id, pre_names)
        .with_causal_inlining()
        .with_temporary_namespace(format!("clocked{}", clock.index()));
    let causal = CausalReadExpansion::new(view, definitions);
    let shared_calls = lower_clock_domain_shared_calls(
        view,
        clock,
        include_unclocked_actions,
        allow_scheduled_shared_calls,
        &causal,
        &mut lowerer,
        &mut pending,
    )?;
    lower_discrete_value_owners(
        &mut DiscreteValueLowering {
            view,
            clock,
            by_id,
            lowerer: &mut lowerer,
            shared_calls: &shared_calls,
        },
        include_unclocked_actions,
        &mut pending,
    )?;
    lower_discrete_real_equations(
        view,
        clock,
        by_id,
        &mut pending,
        &mut lowerer,
        &shared_calls,
    )?;
    locals.extend(lowerer.take_temporary_locals());
    called_user_functions.extend(lowerer.take_called_user_functions());
    lower_event_actions(
        view,
        definitions,
        clock,
        by_id,
        pre_names,
        include_unclocked_actions,
        &mut pending,
    )?;
    for assignment in &mut pending {
        assignment.reads = causal.expand(std::mem::take(&mut assignment.reads));
    }
    if shared_calls.scheduled && !scheduled_sharing_preserves_arguments(&pending, &shared_calls) {
        return lower_clocked_assignments_for_domain(
            view,
            definitions,
            clock,
            by_id,
            pre_names,
            include_unclocked_actions,
            false,
        );
    }
    #[cfg(test)]
    let statements = order_assignments(&pending)?;
    Ok(ClockedAssignments {
        #[cfg(test)]
        statements,
        locals,
        called_user_functions,
        assignments: pending,
        schedules_shared_calls: shared_calls.scheduled,
    })
}

/// Prove that one evaluation of the scheduled shared calls is the value every
/// consumer would have computed for itself.
///
/// Sharing is legal only when no assignment writes an argument of a shared call
/// between two of its consumers. A consumer that writes one of those arguments
/// itself is exactly that case: its own lowering evaluates the call from the
/// prefix, ahead of its stores, while a consumer scheduled after it would have
/// read the stored value. Nothing else can interleave, because the scheduled
/// node reads what the call reads and every consumer reads the node: any writer
/// of an argument is therefore ordered before the node, and the node before
/// every consumer.
fn scheduled_sharing_preserves_arguments(
    pending: &[ClockedAssignment],
    shared: &SharedClockCalls<'_>,
) -> bool {
    let nodes = shared.owners.values().copied().collect::<HashSet<_>>();
    nodes.iter().all(|id| {
        let Some(node) = pending
            .iter()
            .find(|assignment| assignment.targets.contains(id))
        else {
            return true;
        };
        !pending
            .iter()
            .filter(|assignment| assignment.reads.contains(id))
            .any(|consumer| !consumer.targets.is_disjoint(&node.reads))
    })
}

/// Materialize each repeated eager call of one clock domain exactly once.
///
/// A call occurring in more than one value of the domain is evaluated once and
/// its result temporaries are handed to every later occurrence. Three
/// placements are available, and a call takes the first one it qualifies for.
///
/// **Domain entry.** The preamble runs before every clocked assignment of its
/// domain, so a call may only be hoisted there when nothing it reads is written
/// by a clocked assignment in this `DoStep`. That read set must be taken
/// through the same [`CausalReadExpansion`] the scheduler uses: a call that
/// names only algebraic coordinates can still reach discrete state through
/// causal algebraic inlining, and hoisting it would evaluate it against the
/// previous tick's value. Admitting only domain-entry-stable calls also keeps
/// the preamble free of incoming schedule edges, so the barrier every
/// assignment in the domain places on it can never close a cycle.
///
/// **A scheduled position of its own.** A call whose arguments ARE written in
/// this `DoStep` cannot move to domain entry, but it does not have to: the
/// ordering it needs is the one the clock scheduler already derives for
/// ordinary assignments. Such calls are materialized into one node that reads
/// what they read and writes one synthetic schedule index
/// ([`ScheduleIdAllocator`]); every consumer group declares a read of that
/// index. The worklist in `clock_schedule` then places the node after every
/// assignment that writes an argument and before every consumer, which is the
/// ordering the preamble could not express.
///
/// **A scheduled position under the call's own guard.** A value selected by a
/// runtime guard rather than by the domain clock evaluates its calls only on
/// the ticks that guard holds, and moving one of them anywhere unguarded would
/// evaluate it on ticks the model does not, raising assertions and error
/// signals the model never raises. Such calls are materialized into a node that
/// re-states the identical guard through the same [`lower_action_guard`] every
/// consumer group states it with, so the call still runs on exactly its own
/// ticks. Its result temporaries are offered only to groups carrying that
/// identical activation ([`SharedClockCalls::calls_for`]), never to a group
/// under a weaker guard or none.
///
/// In every case `DoStep` values are the only thing that moves: the emitted
/// statement sequence of a node is the one the consumer would have emitted, so
/// no floating-point operation is reordered and the removed evaluations were
/// bit-identical repeats.
///
/// The remaining obligation, that no writer of an argument runs between two
/// consumers, is discharged by [`scheduled_sharing_preserves_arguments`] once
/// the domain's read and target sets are known.
fn lower_clock_domain_shared_calls<'a, 'b, 'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    include_unclocked: bool,
    allow_scheduled: bool,
    causal: &CausalReadExpansion<'a, 'dae>,
    lowerer: &mut ExpressionLowerer<'b, 'dae>,
    pending: &mut Vec<ClockedAssignment>,
) -> Result<SharedClockCalls<'dae>, GalecTargetError> {
    let mutable_targets = view
        .variables()
        .filter_map(|(id, variable)| {
            matches!(
                variable.identity(),
                dae::VariableIdentity::DiscreteReal(_) | dae::VariableIdentity::DiscreteValue(_)
            )
            .then_some(id.index())
        })
        .collect::<HashSet<_>>();
    let roots = clock_domain_value_roots(view, clock, include_unclocked)?;
    let (preamble_calls, unguarded_calls): (Vec<_>, Vec<_>) =
        repeated_calls(view, causal, &roots, ValueActivation::Entry)
            .into_iter()
            .partition(|call| call.reads.is_disjoint(&mutable_targets));
    let entry_owners = preamble_calls
        .iter()
        .chain(&unguarded_calls)
        .map(|call| call.owner)
        .collect::<HashSet<_>>();
    let mut shared = SharedClockCalls {
        entry: HashMap::new(),
        guarded: HashMap::new(),
        owners: HashMap::new(),
        scheduled: false,
    };
    let mut slots = ScheduleIdAllocator::new(view, clock);
    let has_preamble = materialize_shared_call_group(
        lowerer,
        pending,
        &preamble_calls,
        SharedCallPlacement::Preamble,
    )?;
    if allow_scheduled
        && let Some(id) = slots.next()
        && materialize_shared_call_group(
            lowerer,
            pending,
            &unguarded_calls,
            SharedCallPlacement::Scheduled {
                id,
                requires_preamble: has_preamble,
            },
        )?
    {
        shared.scheduled = true;
        shared
            .owners
            .extend(unguarded_calls.iter().map(|call| (call.owner, id)));
    }
    shared.entry = lowerer.shared_materialized_function_calls();
    if allow_scheduled {
        lower_guarded_shared_calls(
            &mut GuardedShareContext {
                view,
                clock,
                causal,
                lowerer,
                pending,
                slots: &mut slots,
                has_preamble,
                entry_owners,
            },
            &roots,
            &mut shared,
        )?;
    }
    lowerer.expect_scheduled_shared_calls(shared.owners.keys().copied().collect());
    Ok(shared)
}

/// Everything the per-guard half of the shared-call plan works against.
struct GuardedShareContext<'ctx, 'a, 'b, 'dae> {
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    causal: &'ctx CausalReadExpansion<'a, 'dae>,
    lowerer: &'ctx mut ExpressionLowerer<'b, 'dae>,
    pending: &'ctx mut Vec<ClockedAssignment>,
    slots: &'ctx mut ScheduleIdAllocator,
    has_preamble: bool,
    entry_owners: HashSet<u32>,
}

/// Emit one shared-call node per distinct runtime activation of this domain.
///
/// A call is admitted only when every occurrence of it in the domain sits under
/// the one activation the node re-states. An owner seen under two activations,
/// or under a guard as well as at domain entry, is declined: one node cannot
/// re-state two guards, and the memo that hands its temporaries on is keyed by
/// the call owner, not by the guard it ran under.
fn lower_guarded_shared_calls<'dae>(
    context: &mut GuardedShareContext<'_, '_, '_, 'dae>,
    roots: &[DomainValueRoot<'dae>],
    shared: &mut SharedClockCalls<'dae>,
) -> Result<(), GalecTargetError> {
    let activations = guarded_activations(roots);
    let mut confined = HashMap::<u32, Option<ValueActivation<'dae>>>::new();
    for activation in &activations {
        for call in repeated_calls(context.view, context.causal, roots, *activation) {
            let held = confined.entry(call.owner).or_insert(Some(*activation));
            *held = held.filter(|held| held == activation);
        }
    }
    for activation in activations {
        let ValueActivation::Guarded { trigger, guard } = activation else {
            continue;
        };
        let calls = repeated_calls(context.view, context.causal, roots, activation)
            .into_iter()
            .filter(|call| {
                confined.get(&call.owner) == Some(&Some(activation))
                    && !context.entry_owners.contains(&call.owner)
            })
            .collect::<Vec<_>>();
        let (Some(first), Some(id)) = (calls.first(), context.slots.next()) else {
            continue;
        };
        let span = first.span;
        // The guard is lowered on a clone: a declined activation must leave no
        // temporary, cache entry or emitted statement behind.
        let mut trial = context.lowerer.clone();
        trial.begin_shared_call_group(&shared.entry);
        let Some(lowered) = lower_shared_call_guard(
            context.view,
            context.clock,
            context.causal,
            &mut trial,
            trigger,
            guard,
            span,
        )?
        else {
            continue;
        };
        let mut reads = lowered.reads;
        for call in &calls {
            trial.materialize_eager_call(call.call)?;
            reads.extend(call.reads.iter().copied());
        }
        let body = trial.drain_prefix_statements();
        if body.is_empty() {
            continue;
        }
        let mut statements = lowered.prefix;
        statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![gast::IfBranch {
                    condition: gast::Condition::Expression(lowered.condition),
                    body,
                    span,
                }],
                else_body: None,
            }),
            span,
        ));
        shared
            .guarded
            .insert(activation, trial.shared_materialized_function_calls());
        *context.lowerer = trial;
        context.pending.push(ClockedAssignment {
            targets: HashSet::from([id]),
            reads,
            statements,
            span,
            is_preamble: false,
            requires_preamble: context.has_preamble,
        });
        shared.scheduled = true;
        shared
            .owners
            .extend(calls.iter().map(|call| (call.owner, id)));
    }
    Ok(())
}

/// The distinct runtime activations this domain's values carry, in a stable
/// order so the emitted nodes do not depend on hash iteration.
fn guarded_activations<'dae>(roots: &[DomainValueRoot<'dae>]) -> Vec<ValueActivation<'dae>> {
    let mut activations = roots
        .iter()
        .map(|root| root.activation)
        .filter(|activation| !matches!(activation, ValueActivation::Entry))
        .collect::<Vec<_>>();
    activations.sort_by_key(|activation| match activation {
        ValueActivation::Entry => (0, 0),
        ValueActivation::Guarded { trigger, guard } => (trigger.index(), guard.index()),
    });
    activations.dedup();
    activations
}

/// The guard a shared-call node re-states, lowered exactly as its consumer
/// groups lower it.
struct SharedCallGuard {
    prefix: Vec<gast::Spanned<gast::Statement>>,
    condition: gast::Expression,
    reads: HashSet<u32>,
}

/// Lower one activation into the guard a shared-call node re-states.
///
/// This is the same [`lower_action_guard`] over the same trigger and guard
/// conditions `lower_discrete_real_group` states around every consumer, so the
/// node runs on exactly the ticks its consumers run on. A trigger this `DoStep`
/// does not own declines, because the consumers refuse it too; a guard that
/// turns out to add no runtime condition declines as well, because such a value
/// belongs at domain entry and must not acquire a guarded node.
fn lower_shared_call_guard<'a, 'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    causal: &CausalReadExpansion<'a, 'dae>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    trigger: dae::ConditionId<'dae>,
    guard: dae::ConditionId<'dae>,
    span: Span,
) -> Result<Option<SharedCallGuard>, GalecTargetError> {
    if require_periodic_trigger(view, trigger, clock, span).is_err() {
        return Ok(None);
    }
    let condition = lower_action_guard(
        &mut ActionGuardContext {
            view,
            expected: clock,
            lowerer,
            span,
        },
        guard,
    )?;
    let Some(condition) = condition else {
        return Ok(None);
    };
    let mut reads = HashSet::new();
    collect_condition_current_reads(view, trigger, &mut reads);
    collect_condition_current_reads(view, guard, &mut reads);
    Ok(Some(SharedCallGuard {
        prefix: lowerer.drain_prefix_statements(),
        condition,
        reads: causal.expand(reads),
    }))
}

/// One repeated call of one activation, with everything its node needs.
struct RepeatedCall<'dae> {
    call: dae::ExprId<'dae>,
    owner: u32,
    reads: HashSet<u32>,
    span: Span,
}

/// The calls this domain evaluates more than once under `activation`.
///
/// Occurrences are counted once per root, so a call two values reach is
/// repeated even when both reach it through one shared expression identity.
/// Call identity is the construction-issued owner, never a result projection.
fn repeated_calls<'a, 'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalReadExpansion<'a, 'dae>,
    roots: &[DomainValueRoot<'dae>],
    activation: ValueActivation<'dae>,
) -> Vec<RepeatedCall<'dae>> {
    let mut occurrences = HashMap::<u32, (dae::ExprId<'dae>, usize)>::new();
    for root in roots.iter().filter(|root| root.activation == activation) {
        let mut seen = HashSet::new();
        collect_eager_calls(view, root.value, causal, &mut seen, &mut occurrences);
    }
    let mut calls = occurrences
        .into_iter()
        .filter(|(_, (_, count))| *count >= 2)
        .map(|(owner, (call, _))| {
            let mut reads = HashSet::new();
            collect_current_reads(view, call, &mut reads);
            RepeatedCall {
                call,
                owner,
                reads: causal.expand(reads),
                span: view
                    .expression(call)
                    .expect("checked eager call resolves")
                    .provenance()
                    .span(),
            }
        })
        .collect::<Vec<_>>();
    calls.sort_by_key(|call| call.call.index());
    calls
}

/// Where one group of shared calls is emitted.
enum SharedCallPlacement {
    Preamble,
    Scheduled { id: u32, requires_preamble: bool },
}

/// Emit one group of shared calls as a single schedulable statement block, and
/// report whether it produced one.
///
/// The whole group lands in one block, in issued order, exactly as the domain
/// preamble has always emitted its calls. Keeping the group undivided is what
/// makes the block safe to move: a temporary one call's argument lowering
/// creates is read by the next call in the same block, never across a
/// schedulable boundary.
fn materialize_shared_call_group<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    pending: &mut Vec<ClockedAssignment>,
    calls: &[RepeatedCall<'dae>],
    placement: SharedCallPlacement,
) -> Result<bool, GalecTargetError> {
    let Some(first) = calls.first() else {
        return Ok(false);
    };
    let span = first.span;
    let mut reads = HashSet::new();
    for call in calls {
        lowerer.materialize_eager_call(call.call)?;
        reads.extend(call.reads.iter().copied());
    }
    let statements = lowerer.drain_prefix_statements();
    if statements.is_empty() {
        return Ok(false);
    }
    let (targets, is_preamble, requires_preamble) = match placement {
        SharedCallPlacement::Preamble => (HashSet::new(), true, false),
        SharedCallPlacement::Scheduled {
            id,
            requires_preamble,
        } => (HashSet::from([id]), false, requires_preamble),
    };
    pending.push(ClockedAssignment {
        targets,
        reads,
        statements,
        span,
        is_preamble,
        requires_preamble,
    });
    Ok(true)
}

/// The synthetic schedule indices one clock domain's shared-call nodes own.
///
/// Checked DAE variable indices are dense from zero, so indices allocated above
/// `variable_count` are disjoint from every checked variable a read set can
/// name; striding by the checked clock count keeps two domains' allocations
/// disjoint from each other. A model large enough to exhaust `u32` here simply
/// gets no scheduled sharing.
struct ScheduleIdAllocator {
    base: Option<u32>,
    stride: u32,
    slot: u32,
}

impl ScheduleIdAllocator {
    fn new(view: dae::DaeView<'_>, clock: dae::ClockId<'_>) -> Self {
        Self {
            base: u32::try_from(view.variable_count())
                .ok()
                .and_then(|count| count.checked_add(clock.index())),
            stride: u32::try_from(view.clock_count()).unwrap_or(u32::MAX),
            slot: 0,
        }
    }

    fn next(&mut self) -> Option<u32> {
        let id = self
            .base?
            .checked_add(self.slot.checked_mul(self.stride)?)?;
        self.slot = self.slot.checked_add(1)?;
        Some(id)
    }
}
/// One value this clock domain evaluates, and the activation it evaluates
/// under.
struct DomainValueRoot<'dae> {
    value: dae::ExprId<'dae>,
    activation: ValueActivation<'dae>,
}

/// Every value this clock domain evaluates, classified by activation.
///
/// A discrete-value owner branch contributes only `Entry` roots. Its consumer
/// lowering hands out the entry memo alone, so a guarded call it names would
/// gain a node no consumer could read; counting such an occurrence would let a
/// call with one real consumer look repeated. Discrete-value branches under a
/// runtime guard are therefore left out of the scan entirely.
fn clock_domain_value_roots<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    include_unclocked: bool,
) -> Result<Vec<DomainValueRoot<'dae>>, GalecTargetError> {
    let mut roots = Vec::new();
    let real_clocks = discrete_real_clock_owners(view);
    let causal_plan = causal_discrete_plan(view)?;
    for (index, equation) in view.discrete_real_equations().enumerate() {
        let Some(definition) = causal_plan.discrete_real_definition(index) else {
            continue;
        };
        let target = dae::VariableId::from(definition.target());
        if require_discrete_real_clock_owner(&real_clocks, target, equation.provenance().span())?
            != clock.index()
        {
            continue;
        }
        roots.push(DomainValueRoot {
            value: definition.value(),
            activation: value_activation(view, equation.activation(), clock),
        });
    }
    let value_clocks = discrete_value_clock_owners(view);
    for index in 0..view.discrete_value_owner_count() {
        let owner = view
            .discrete_value_owner(
                view.discrete_value_owner_id(index)
                    .expect("dense checked B.1c owner identity"),
            )
            .expect("checked B.1c owner resolves");
        if !discrete_value_owner_runs_in_domain(
            view,
            owner,
            clock,
            include_unclocked,
            &value_clocks,
        )? {
            continue;
        }
        for branch in owner.branches().iter() {
            let selected_by_clock = match branch.activation() {
                dae::DiscreteBranchActivation::Always => true,
                dae::DiscreteBranchActivation::When { guard, .. } => {
                    condition_is_domain_clock_only(view, guard, clock)
                }
            };
            if selected_by_clock {
                roots.extend(branch.values().iter().map(|(value, _)| DomainValueRoot {
                    value,
                    activation: ValueActivation::Entry,
                }));
            }
        }
    }
    Ok(roots)
}

/// Classify one clocked definition's activation.
///
/// `Always`, and a `when` whose guard names nothing but this domain's clock,
/// add no runtime condition inside the already-selected `DoStep`, so their
/// values are `Entry` and keep the domain-entry placement they have always
/// had. Everything else carries its trigger and guard identities, which is what
/// a node must re-state to run on the same ticks.
fn value_activation<'dae>(
    view: dae::DaeView<'dae>,
    activation: dae::DiscreteRealActivation<'dae>,
    clock: dae::ClockId<'dae>,
) -> ValueActivation<'dae> {
    match activation {
        dae::DiscreteRealActivation::Always => ValueActivation::Entry,
        dae::DiscreteRealActivation::When { trigger, guard } => {
            if condition_is_domain_clock_only(view, guard, clock) {
                ValueActivation::Entry
            } else {
                ValueActivation::Guarded { trigger, guard }
            }
        }
    }
}

/// Prove that evaluating a value at domain entry cannot cross a runtime guard.
///
/// `Clock` and `Always` add no condition inside the already-selected DoStep
/// domain. Relations, discrete predicates, negation, and disjunction remain
/// lazy and therefore forbid preamble hoisting.
fn condition_is_domain_clock_only<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    clock: dae::ClockId<'dae>,
) -> bool {
    match view
        .condition(condition)
        .expect("checked preamble condition resolves")
        .operation()
    {
        dae::ConditionOperation::Clock(found) => found == clock,
        dae::ConditionOperation::Always => true,
        dae::ConditionOperation::And(lhs, rhs) => {
            condition_is_domain_clock_only(view, lhs, clock)
                && condition_is_domain_clock_only(view, rhs, clock)
        }
        dae::ConditionOperation::Initial
        | dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_)
        | dae::ConditionOperation::Not(_)
        | dae::ConditionOperation::Or(_, _)
        | dae::ConditionOperation::AnyRise(_, _) => false,
    }
}

/// Count the eager call occurrences of one clock-domain value, following the
/// causal definitions the `DoStep` lowering substitutes for an algebraic
/// coordinate.
///
/// The lowered value is not the DAE value: with causal inlining on, an
/// algebraic coordinate is replaced by its definition
/// ([`ExpressionLowerer::inline_algebraic_coordinate`]), so a call named only
/// once in the model is evaluated once per value that reaches it through such a
/// coordinate. Stopping the scan at the coordinate reports those occurrences as
/// one, and the repeated call is never recognised.
fn collect_eager_calls<'a, 'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    causal: &CausalReadExpansion<'a, 'dae>,
    seen: &mut HashSet<u32>,
    occurrences: &mut HashMap<u32, (dae::ExprId<'dae>, usize)>,
) {
    expression_functions::for_each_eager_call(
        view,
        expression,
        seen,
        &|coordinate| causal.inlined_definition(coordinate),
        &mut |call, owner| {
            // Count construction-issued invocations, not scalar projections.
            occurrences
                .entry(owner)
                .and_modify(|(_, count)| *count += 1)
                .or_insert((call, 1));
        },
    );
}

/// One shared causal-definition proof plus the variable index it is queried by.
///
/// Deriving the proof is whole-model work; every clock domain and every
/// assignment in it asks the same questions, so it is derived once per domain
/// and reused.
struct CausalReadExpansion<'a, 'dae> {
    view: dae::DaeView<'dae>,
    definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
    variables: HashMap<u32, dae::VariableId<'dae>>,
}

impl<'a, 'dae> CausalReadExpansion<'a, 'dae> {
    fn new(
        view: dae::DaeView<'dae>,
        definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
    ) -> Self {
        Self {
            definitions,
            variables: view.variables().map(|(id, _)| (id.index(), id)).collect(),
            view,
        }
    }

    /// Expand current-tick reads through every exact acyclic algebraic
    /// definition.
    ///
    /// Clock-domain ordering cannot stop at an intermediate algebraic
    /// coordinate: if `command = filtered` and `filtered = slowState`, the fast
    /// domain reads `slowState` even though its lowered expression initially
    /// names `filtered`. An array assembled one element at a time
    /// (`alias[1] = slowState; alias[2] = 0;`) hides the same edge behind a
    /// complete scalar definition set rather than one whole-variable
    /// definition, so both forms are followed.
    /// [`rumoca_phase_structural::CausalDefinitions`] is the construction proof
    /// that this traversal is finite and semantics-preserving.
    fn expand(&self, mut reads: HashSet<u32>) -> HashSet<u32> {
        let mut pending = reads.iter().copied().collect::<Vec<_>>();
        let mut expanded = HashSet::new();
        while let Some(index) = pending.pop() {
            if !expanded.insert(index) {
                continue;
            }
            let Some(&variable) = self.variables.get(&index) else {
                continue;
            };
            let mut definition_reads = HashSet::new();
            self.collect_definition_reads(variable, &mut definition_reads);
            pending.extend(
                definition_reads
                    .into_iter()
                    .filter(|dependency| reads.insert(*dependency)),
            );
        }
        reads
    }

    /// The expression the `DoStep` lowering substitutes for one coordinate.
    ///
    /// Only a whole-variable algebraic definition answers, because that is the
    /// substitution [`ExpressionLowerer::inline_algebraic_coordinate`] takes
    /// without knowing which coordinate is being projected. A per-scalar
    /// definition set answers `None`: which of its members a use reaches
    /// depends on the index, so admitting them here would count a call the
    /// value never evaluates.
    fn inlined_definition(
        &self,
        coordinate: dae::CoordinateView<'dae>,
    ) -> Option<dae::ExprId<'dae>> {
        match coordinate {
            dae::CoordinateView::Algebraic(variable) => self.definitions.definition(variable),
            _ => None,
        }
    }

    fn collect_definition_reads(&self, variable: dae::VariableId<'dae>, reads: &mut HashSet<u32>) {
        if let Some(definition) = self.definitions.definition_for_variable(variable) {
            collect_current_reads(self.view, definition, reads);
            return;
        }
        if !self.definitions.fully_defines_variable(variable) {
            return;
        }
        let Some(scalar_count) = self
            .view
            .variable(variable)
            .and_then(|declaration| u32::try_from(declaration.scalar_count()).ok())
        else {
            return;
        };
        for scalar in 0..scalar_count {
            if let Some(definition) = self
                .definitions
                .scalar_definition_for_variable(variable, scalar)
            {
                collect_current_reads(self.view, definition, reads);
            }
        }
    }
}

/// One planned clocked discrete-`Real` definition, before emission.
struct PlannedDiscreteReal<'refs, 'dae> {
    target: dae::VariableId<'dae>,
    value: dae::ExprId<'dae>,
    span: Span,
    activation: dae::DiscreteRealActivation<'dae>,
    classified: &'refs ClassifiedVariable<'dae>,
}

/// The construction-issued call occurrence a definition value projects.
///
/// DAE-C21: the N result projections of one multi-output call all carry the
/// same issued `Call.owner`. Reading that issued identity is the only
/// admissible way to recognise them as one invocation — never the callee body,
/// name, span, argument shape, or an expression comparison.
fn projected_call_owner<'dae>(view: dae::DaeView<'dae>, value: dae::ExprId<'dae>) -> Option<u32> {
    let mut current = value;
    loop {
        match view.expression(current)?.operation() {
            dae::ExpressionOperation::Call { owner, .. } => return Some(owner.index()),
            dae::ExpressionOperation::Field { base, .. }
            | dae::ExpressionOperation::Index { base, .. } => current = base,
            _ => return None,
        }
    }
}

fn activation_key(activation: dae::DiscreteRealActivation<'_>) -> (u32, u32) {
    match activation {
        dae::DiscreteRealActivation::Always => (u32::MAX, u32::MAX),
        dae::DiscreteRealActivation::When { trigger, guard } => (trigger.index(), guard.index()),
    }
}

fn plan_clocked_discrete_reals<'refs, 'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &'refs HashMap<u32, ClassifiedVariable<'dae>>,
) -> Result<Vec<PlannedDiscreteReal<'refs, 'dae>>, GalecTargetError> {
    let clock_owners = discrete_real_clock_owners(view);
    let causal_plan = causal_discrete_plan(view)?;
    let mut planned = Vec::new();
    for (index, equation) in view.discrete_real_equations().enumerate() {
        let Some(definition) = causal_plan.discrete_real_definition(index) else {
            continue;
        };
        let span = equation.provenance().span();
        let target = dae::VariableId::from(definition.target());
        if require_discrete_real_clock_owner(&clock_owners, target, span)? != clock.index() {
            continue;
        }
        let classified = by_id.get(&target.index()).ok_or_else(|| {
            GalecTargetError::UnknownVariableReference {
                name: format!("#{}", target.index()),
                span: Some(span),
            }
        })?;
        planned.push(PlannedDiscreteReal {
            target,
            value: definition.value(),
            span,
            activation: equation.activation(),
            classified,
        });
    }
    Ok(planned)
}

/// Partition planned definitions into one emission group per issued call
/// occurrence, preserving first-definition order.
///
/// Grouping is what makes one source invocation emit one call: the intra-group
/// materialization memo is only severed at a group boundary
/// (`finish_statement_group`), so N projections spread over N groups become N
/// calls. A target with more than one definition keeps its own group, because
/// the conditional-owner merge below rewrites that group in place.
fn group_planned_discrete_reals<'dae>(
    view: dae::DaeView<'dae>,
    planned: &[PlannedDiscreteReal<'_, 'dae>],
) -> Vec<Vec<usize>> {
    let mut definition_counts: HashMap<u32, usize> = HashMap::new();
    for plan in planned {
        *definition_counts.entry(plan.target.index()).or_default() += 1;
    }
    let mut groups: Vec<Vec<usize>> = Vec::new();
    let mut open: HashMap<(u32, (u32, u32)), usize> = HashMap::new();
    for (slot, plan) in planned.iter().enumerate() {
        let key = (definition_counts.get(&plan.target.index()) == Some(&1))
            .then(|| projected_call_owner(view, plan.value))
            .flatten()
            .map(|owner| (owner, activation_key(plan.activation)));
        match key.and_then(|key| open.get(&key).copied()) {
            Some(existing) => groups[existing].push(slot),
            None => {
                if let Some(key) = key {
                    open.insert(key, groups.len());
                }
                groups.push(vec![slot]);
            }
        }
    }
    groups
}

fn lower_discrete_real_equations<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pending: &mut Vec<ClockedAssignment>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    shared_calls: &SharedClockCalls<'dae>,
) -> Result<(), GalecTargetError> {
    let planned = plan_clocked_discrete_reals(view, clock, by_id)?;
    let groups = group_planned_discrete_reals(view, &planned);
    let mut owners: HashMap<u32, (usize, bool)> = HashMap::new();
    for group in &groups {
        let assignment = lower_discrete_real_group(
            view,
            clock,
            lowerer,
            shared_calls,
            &planned,
            group.as_slice(),
        )?;
        merge_discrete_real_assignment(
            pending,
            &mut owners,
            &planned,
            group.as_slice(),
            assignment,
        )?;
    }
    Ok(())
}

/// Emit one group of planned definitions as a single schedulable assignment.
///
/// Every definition in the group is lowered before the prefix is drained, so
/// the shared call materializes once and the later projections read its result
/// temporaries.
fn lower_discrete_real_group<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    shared_calls: &SharedClockCalls<'dae>,
    planned: &[PlannedDiscreteReal<'_, 'dae>],
    group: &[usize],
) -> Result<ClockedAssignment, GalecTargetError> {
    let head = &planned[group[0]];
    let span = head.span;
    // The memo this group may restore is the one written under its own
    // activation. Installing it before the guard lowers is what keeps a guard,
    // which is lowered ahead of the group's first statement boundary, from
    // taking a temporary a node under a different activation wrote.
    let activation = value_activation(view, head.activation, clock);
    let shared = shared_calls.calls_for(activation).clone();
    lowerer.begin_shared_call_group(&shared);
    let (guard, guard_prefix) = match head.activation {
        dae::DiscreteRealActivation::Always => (None, Vec::new()),
        dae::DiscreteRealActivation::When { trigger, guard } => {
            require_periodic_trigger(view, trigger, clock, span)?;
            let guard = lower_action_guard(
                &mut ActionGuardContext {
                    view,
                    expected: clock,
                    lowerer,
                    span,
                },
                guard,
            )?;
            let prefix = lowerer.take_prefix_statements_with_shared_calls(&shared);
            (guard, prefix)
        }
    };
    let mut assignments = Vec::new();
    let mut reads = HashSet::new();
    let mut targets = HashSet::new();
    for &slot in group {
        let plan = &planned[slot];
        targets.insert(plan.target.index());
        collect_current_reads(view, plan.value, &mut reads);
        if let dae::DiscreteRealActivation::When { trigger, guard } = plan.activation {
            collect_condition_current_reads(view, trigger, &mut reads);
            collect_condition_current_reads(view, guard, &mut reads);
        }
        append_definition_assignments(
            lowerer,
            plan.value,
            plan.classified,
            plan.span,
            &mut assignments,
        )?;
    }
    // A merged group writes its own targets in causal order, so a read of one
    // of them by a later member is satisfied inside the group exactly as the
    // scheduler would have satisfied it between the separate groups. Retaining
    // such a read would be a self-dependency and would reject a valid model.
    // An unmerged group keeps its read set byte-for-byte as before, so a single
    // definition's self-read still reaches the scheduler unchanged.
    if group.len() > 1 {
        for target in &targets {
            reads.remove(target);
        }
    }
    let mut body = lowerer.take_prefix_statements_with_shared_calls(&shared);
    body.extend(assignments);
    let statements = match guard {
        Some(condition) => {
            let mut statements = guard_prefix;
            statements.push(gast::Spanned::new(
                gast::Statement::If(gast::IfStatement {
                    branches: vec![gast::IfBranch {
                        condition: gast::Condition::Expression(condition),
                        body,
                        span,
                    }],
                    else_body: None,
                }),
                span,
            ));
            statements
        }
        None => body,
    };
    // A group that took a scheduled shared call's result temporaries reads the
    // node that wrote them, so the scheduler keeps that node ahead of it.
    let consumed = lowerer.take_consumed_scheduled_calls();
    reads.extend(shared_calls.consumed_reads(&consumed).collect::<Vec<_>>());
    Ok(ClockedAssignment {
        targets,
        reads,
        statements,
        span,
        is_preamble: false,
        requires_preamble: !shared.is_empty(),
    })
}

/// Emit one checked clocked definition into `assignments`.
///
/// A rank-`n` target is normally projected coordinate by coordinate, which is
/// correct but turns one source definition into `product(dimensions)` Algorithm
/// Code assignments — 225 of them for a 15x15 covariance. When the definition's
/// value already denotes ONE array-shaped GALEC storage object of exactly the
/// target's shape and element type, that whole coordinate set is by
/// construction the identity map of the source's coordinates onto the target's,
/// under the one row-major order both sides are subscripted with. One
/// whole-array assignment is then constructed in place of the set.
///
/// This is emission and not recognition (TRP-020/021/035): the decision is
/// taken from the checked DAE value BEFORE any coordinate statement exists, and
/// no already-emitted statement is inspected, matched or rewritten. When the
/// whole-array form is not proven the coordinate projection runs exactly as
/// before.
fn append_definition_assignments<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    value: dae::ExprId<'dae>,
    classified: &ClassifiedVariable<'dae>,
    span: Span,
    assignments: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let dimensions = classified.variable.value_type().dimensions();
    if let Some(source) = whole_array_definition_source(lowerer, value, classified)? {
        assignments.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: state_reference_indexed(classified.name.clone(), &[], span),
                value: source,
            },
            span,
        ));
        return Ok(());
    }
    if let Some(statements) = tensor_definition_assignments(lowerer, value, classified, span) {
        assignments.extend(statements);
        return Ok(());
    }
    for indices in row_major_indices(dimensions) {
        let lowered = lowerer.lower_element(value, &indices)?;
        let value = coerce(lowered, classified.scalar_type, span)?;
        assignments.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: state_reference_indexed(classified.name.clone(), &indices, span),
                value,
            },
            span,
        ));
    }
    Ok(())
}

/// Lower one rank-`n` clocked definition through the function-body tensor
/// path, when its value carries a whole-array contraction that the coordinate
/// projection would dissolve (reconstruction ledger R-9).
///
/// The value is lowered ONCE at symbolic axis iterators, exactly as a
/// function-body whole-array assignment is, so a contraction keeps its
/// free-index loops and its hoisted invariant half instead of re-deriving the
/// contraction per target coordinate. The decision is taken from the checked
/// DAE value before any coordinate statement exists
/// ([`expression_projection::contains_whole_array_contraction`]), so this is
/// emission and not recognition (TRP-020/021/035).
///
/// Every precondition fails closed to the coordinate projection:
///
/// * A value without a matrix-involving contraction keeps the coordinate
///   form byte for byte, so equations that never dissolve are untouched.
/// * A value that reads its own target's current tick keeps the coordinate
///   form. (Such a self-read is a scheduling cycle and is rejected later,
///   but this path must not change WHICH refusal the model receives.)
/// * A value the symbolic-index projection cannot lower (for example an
///   array constructor selected per coordinate) keeps the coordinate form:
///   the trial runs on a clone of the lowerer, so a failed attempt leaves no
///   temporary, cache entry or emitted statement behind.
fn tensor_definition_assignments<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    value: dae::ExprId<'dae>,
    classified: &ClassifiedVariable<'dae>,
    span: Span,
) -> Option<Vec<gast::Spanned<gast::Statement>>> {
    if classified.variable.value_type().dimensions().is_empty()
        || !expression_projection::contains_whole_array_contraction(lowerer.view, value)
    {
        return None;
    }
    let mut current_reads = HashSet::new();
    collect_current_reads(lowerer.view, value, &mut current_reads);
    if current_reads.contains(&classified.id.index()) {
        return None;
    }
    let mut trial = lowerer.clone();
    match lower_tensor_definition(&mut trial, value, classified, span) {
        Ok(statements) => {
            *lowerer = trial;
            Some(statements)
        }
        Err(_) => None,
    }
}

/// The tensor-loop statement sequence for one clocked whole-array definition:
/// the mirror of `user_functions::lower_tensor_function_assignment`, storing
/// to checked state instead of a function local.
///
/// No snapshot prologue is needed here: the caller has already proven the
/// value does not read the target's current tick, so the store can never
/// observe its own writes.
fn lower_tensor_definition<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    value: dae::ExprId<'dae>,
    classified: &ClassifiedVariable<'dae>,
    span: Span,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let dimensions = classified.variable.value_type().dimensions();
    user_functions::materialize_eager_aggregate_calls(value, lowerer)?;
    let names = dimensions
        .iter()
        .enumerate()
        .map(|(axis, _)| {
            gast::Name::ident(format!(
                "rumoca_tensor_{}_{}_{}",
                lowerer.temporary_namespace, lowerer.temporary_counter, axis
            ))
        })
        .collect::<Vec<_>>();
    lowerer.temporary_counter += 1;
    let bounds_depth = lowerer.loop_index_bounds.len();
    for (name, &extent) in names.iter().zip(dimensions) {
        lowerer.loop_index_bounds.push(LoopIndexBound {
            name: name.clone(),
            minimum: 1,
            maximum: i64::from(extent),
        });
    }
    let indices = names
        .iter()
        .cloned()
        .map(|name| gast::Expression::Ref(gast::Reference::local(name)))
        .collect::<Vec<_>>();
    let prefix_start = lowerer.pending_prefix_statements.len();
    let lowered = lowerer.lower_at(value, &indices);
    lowerer.loop_index_bounds.truncate(bounds_depth);
    let lowered = coerce(lowered?, classified.scalar_type, span)?;
    let prefixes = lowerer.pending_prefix_statements.split_off(prefix_start);
    let (before, mut body) = user_functions::partition_tensor_prefixes(prefixes, &names);
    lowerer.pending_prefix_statements.extend(before);
    body.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference_with_subscripts(classified.name.clone(), indices, span),
            value: lowered,
        },
        span,
    ));
    Ok(user_functions::nest_tensor_loops(
        body,
        &names,
        &expression_projection::AxisBounds {
            extents: dimensions,
            proven: &|index, extent| lowerer.prove_dynamic_index(index, extent, span).is_ok(),
        },
        span,
    ))
}

/// The single array-shaped GALEC storage object `value` already denotes, when
/// copying it whole is provably the coordinate projection this definition would
/// otherwise emit.
///
/// The proof obligations, each of which fails closed:
///
/// * **Rank.** A rank-0 target has no coordinate set to collapse.
/// * **Correspondence.** The value's checked shape must equal the target's,
///   extent by extent. Both sides are then subscripted by the same
///   [`row_major_indices`] sequence, so coordinate `k` of the source is
///   coordinate `k` of the target — a permuted, transposed, offset or
///   differently-extented source never reaches this form because it is not
///   spelled as a bare reference to one whole object.
/// * **Completeness.** [`row_major_indices`] enumerates every coordinate of the
///   target exactly once, and the whole-array assignment replaces exactly that
///   enumeration, so no coordinate is dropped, added or written twice.
/// * **Element type.** `coerce` may insert a per-coordinate `real(...)`
///   conversion, which a whole-array copy cannot express, so a differing scalar
///   type keeps the coordinate form.
/// * **Single source, no interleaving.** Only the two operations below can
///   answer: a directly-lowerable call, whose materialized result temporary is
///   one object written by one prefix statement that already dominates every
///   coordinate of this group; and an expression that names whole checked
///   storage. Both replace one contiguous coordinate run of one definition, so
///   nothing is reordered across a neighbouring definition or a guard.
/// * **Aliasing.** GALEC whole-array assignment and the coordinate projection
///   it replaces both copy forward in row-major order, so an overlapping source
///   and target produce the same values either way.
fn whole_array_definition_source<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    value: dae::ExprId<'dae>,
    classified: &ClassifiedVariable<'dae>,
) -> Result<Option<gast::Expression>, GalecTargetError> {
    let dimensions = classified.variable.value_type().dimensions();
    if dimensions.is_empty() {
        return Ok(None);
    }
    let node = lowerer
        .view
        .expression(value)
        .expect("checked clocked definition value resolves");
    let value_type = node.value_type();
    if value_type.is_record() || value_type.dimensions() != dimensions {
        return Ok(None);
    }
    let span = node.provenance().span();
    let source_scalar = scalar_type(
        value_type.scalar_type(),
        classified.variable.name().as_str(),
        span,
    )?;
    if source_scalar != classified.scalar_type {
        return Ok(None);
    }
    // A directly lowerable call's selected result IS a materialized temporary of
    // the result's own shape. Asking for it at rank `n` rather than per
    // coordinate reuses the same memoized `MultiAssignment` prefix the
    // coordinate projection would have emitted, so the call still happens
    // exactly once and still dominates this assignment.
    if let dae::ExpressionOperation::Call {
        function,
        output,
        arguments,
        ..
    } = node.operation()
        && lowerer.materialize_function_values
        && user_functions::is_directly_lowerable(lowerer.view, function)
    {
        let lowered = lowerer.lower_call_at(value, function, output, arguments, &[], span)?;
        if lowered.scalar_type != classified.scalar_type {
            return Ok(None);
        }
        return Ok(Some(lowered.expression));
    }
    // Otherwise only an expression that already names whole checked storage
    // qualifies. Everything computed — a negation, a sum, a conditional, an
    // array constructor, an inlined causal local — answers `None` here and
    // keeps its coordinate projection.
    lowerer.direct_whole_aggregate_reference(value)
}

fn merge_discrete_real_assignment<'dae>(
    pending: &mut Vec<ClockedAssignment>,
    owners: &mut HashMap<u32, (usize, bool)>,
    planned: &[PlannedDiscreteReal<'_, 'dae>],
    group: &[usize],
    assignment: ClockedAssignment,
) -> Result<(), GalecTargetError> {
    let head = &planned[group[0]];
    let unconditional = matches!(head.activation, dae::DiscreteRealActivation::Always);
    // Only single-definition targets are grouped, so a repeated definition is
    // always a one-member group and keeps the established conditional-owner
    // merge.
    if let Some(&(owner, owner_unconditional)) = owners.get(&head.target.index()) {
        if unconditional || owner_unconditional {
            return Err(unsupported(
                "multiple-discrete-real-definitions",
                format!(
                    "discrete Real `{}` has multiple definitions without one conditional owner",
                    head.classified.variable.name()
                ),
                head.span,
            ));
        }
        pending[owner].reads.extend(assignment.reads);
        pending[owner].statements.extend(assignment.statements);
        return Ok(());
    }
    let index = pending.len();
    for &slot in group {
        owners.insert(planned[slot].target.index(), (index, unconditional));
    }
    pending.push(assignment);
    Ok(())
}

fn causal_discrete_plan<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<rumoca_phase_structural::CausalDiscretePlan<'dae>, GalecTargetError> {
    rumoca_phase_structural::CausalDiscretePlan::derive(view).map_err(|error| {
        let rumoca_phase_structural::CausalDiscreteError::NonComputable { span } = error;
        coupled_discrete_real_equation(span)
    })
}

fn coupled_discrete_real_equation(span: Span) -> GalecTargetError {
    unsupported(
        "coupled-discrete-real-equation",
        "a coupled B.1b residual cannot be represented as one GALEC state assignment".to_owned(),
        span,
    )
}

fn require_discrete_real_clock_owner<'dae>(
    owners: &HashMap<u32, u32>,
    target: dae::VariableId<'dae>,
    span: Span,
) -> Result<u32, GalecTargetError> {
    match owners.get(&target.index()).copied() {
        Some(clock) => Ok(clock),
        None => Err(unsupported(
            "clock-domain",
            "discrete Real definition has no explicit clock owner".to_owned(),
            span,
        )),
    }
}

fn discrete_real_clock_owners(view: dae::DaeView<'_>) -> HashMap<u32, u32> {
    clock_owners_of_kind(view, dae::ClockedVariableKind::DiscreteReal)
}

fn lower_event_actions<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    include_unclocked: bool,
    pending: &mut Vec<ClockedAssignment>,
) -> Result<(), GalecTargetError> {
    let mut lowerer = ExpressionLowerer::with_assertions(view, definitions, by_id, pre_names)
        .with_causal_inlining();
    for (_, action) in view.event_actions() {
        let span = action.provenance().span();
        let dae::EventActionOperation::Assert { level: None, .. } = action.operation() else {
            return Err(unsupported(
                "event-action",
                format!(
                    "event action `{}` cannot be represented in GALEC DoStep",
                    event_name(action.operation())
                ),
                span,
            ));
        };
        let trigger_is_always = matches!(
            view.condition(action.trigger())
                .expect("checked event trigger resolves")
                .operation(),
            dae::ConditionOperation::Always
        );
        let trigger_clocks = condition_clocks(view, action.trigger());
        if trigger_clocks.is_empty() && !include_unclocked {
            continue;
        }
        if !trigger_clocks.is_empty() && !trigger_clocks.contains(&clock.index()) {
            continue;
        }
        if !trigger_is_always {
            require_periodic_trigger(view, action.trigger(), clock, span)?;
        }
        let guard = lower_action_guard(
            &mut ActionGuardContext {
                view,
                expected: clock,
                lowerer: &mut lowerer,
                span,
            },
            action.guard(),
        )?;
        let signal = gast::Spanned::new(
            gast::Statement::Signal(vec![gast::Identifier::new(
                gast::PredefinedSignal::InvalidArgument.name(),
            )]),
            span,
        );
        let mut statements = lowerer.take_prefix_statements();
        statements.extend(match guard {
            Some(condition) => vec![gast::Spanned::new(
                gast::Statement::If(gast::IfStatement {
                    branches: vec![gast::IfBranch {
                        condition: gast::Condition::Expression(condition),
                        body: vec![signal],
                        span,
                    }],
                    else_body: None,
                }),
                span,
            )],
            None => vec![signal],
        });
        let mut reads = HashSet::new();
        collect_condition_current_reads(view, action.trigger(), &mut reads);
        collect_condition_current_reads(view, action.guard(), &mut reads);
        pending.push(ClockedAssignment {
            targets: HashSet::new(),
            reads,
            statements,
            span,
            is_preamble: false,
            requires_preamble: false,
        });
    }
    Ok(())
}

fn condition_clocks<'dae>(view: dae::DaeView<'dae>, root: dae::ConditionId<'dae>) -> HashSet<u32> {
    let mut pending = vec![root];
    let mut seen = HashSet::new();
    let mut clocks = HashSet::new();
    while let Some(condition) = pending.pop() {
        if !seen.insert(condition.index()) {
            continue;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Clock(clock) => {
                clocks.insert(clock.index());
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => pending.extend([lhs, rhs]),
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Relation(_)
            | dae::ConditionOperation::Discrete(_) => {}
        }
    }
    clocks
}

struct DiscreteValueLowering<'context, 'refs, 'dae> {
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &'refs HashMap<u32, ClassifiedVariable<'dae>>,
    lowerer: &'context mut ExpressionLowerer<'refs, 'dae>,
    shared_calls: &'context SharedClockCalls<'dae>,
}

fn lower_discrete_value_owners<'dae>(
    context: &mut DiscreteValueLowering<'_, '_, 'dae>,
    include_unclocked: bool,
    pending: &mut Vec<ClockedAssignment>,
) -> Result<(), GalecTargetError> {
    let clock_owners = discrete_value_clock_owners(context.view);
    for index in 0..context.view.discrete_value_owner_count() {
        let owner = context
            .view
            .discrete_value_owner(
                context
                    .view
                    .discrete_value_owner_id(index)
                    .expect("dense checked B.1c owner identity"),
            )
            .expect("checked B.1c owner resolves");
        if !discrete_value_owner_runs_in_domain(
            context.view,
            owner,
            context.clock,
            include_unclocked,
            &clock_owners,
        )? {
            continue;
        }
        pending.push(lower_discrete_value_owner(context, owner)?);
    }
    Ok(())
}

fn discrete_value_owner_runs_in_domain<'dae>(
    view: dae::DaeView<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
    clock: dae::ClockId<'dae>,
    include_unclocked: bool,
    clock_owners: &HashMap<u32, u32>,
) -> Result<bool, GalecTargetError> {
    let explicit_clocks = owner
        .targets()
        .iter()
        .map(|target| clock_owners.get(&target.index()).copied())
        .collect::<Option<HashSet<_>>>();
    if explicit_clocks
        .as_ref()
        .is_some_and(|clocks| clocks.len() != 1)
    {
        return Err(unsupported(
            "clock-domain",
            "one atomic discrete value owner spans multiple clock domains".to_owned(),
            owner.provenance().span(),
        ));
    }
    let trigger_clocks = owner
        .branches()
        .iter()
        .flat_map(|branch| match branch.activation() {
            dae::DiscreteBranchActivation::When { trigger, .. } => condition_clocks(view, trigger),
            dae::DiscreteBranchActivation::Always => HashSet::new(),
        })
        .collect::<HashSet<_>>();
    let explicit_clocks = explicit_clocks.filter(|clocks| !clocks.is_empty());
    Ok(
        !((explicit_clocks.is_none() && trigger_clocks.is_empty() && !include_unclocked)
            || explicit_clocks
                .as_ref()
                .is_some_and(|clocks| !clocks.contains(&clock.index()))
            || (explicit_clocks.is_none()
                && !trigger_clocks.is_empty()
                && !trigger_clocks.contains(&clock.index()))),
    )
}

fn discrete_value_clock_owners(view: dae::DaeView<'_>) -> HashMap<u32, u32> {
    clock_owners_of_kind(view, dae::ClockedVariableKind::DiscreteValue)
}

/// Owning clock of every checked coordinate of one clocked variable kind.
fn clock_owners_of_kind(
    view: dae::DaeView<'_>,
    kind: dae::ClockedVariableKind,
) -> HashMap<u32, u32> {
    view.clock_ownerships()
        .filter_map(|(_, ownership)| {
            (ownership.kind() == kind)
                .then_some((ownership.variable().index(), ownership.clock().index()))
        })
        .collect()
}

fn lower_discrete_value_owner<'dae>(
    context: &mut DiscreteValueLowering<'_, '_, 'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<ClockedAssignment, GalecTargetError> {
    let span = owner.provenance().span();
    let target_variables = owner
        .targets()
        .iter()
        .map(dae::VariableId::from)
        .collect::<Vec<_>>();
    let classified = target_variables
        .iter()
        .map(|target| {
            context.by_id.get(&target.index()).ok_or_else(|| {
                GalecTargetError::UnknownVariableReference {
                    name: format!("#{}", target.index()),
                    span: Some(span),
                }
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut reads = HashSet::new();
    let mut conditional = Vec::new();
    let mut unconditional = None;
    for branch in owner.branches().iter() {
        let branch_span = branch.provenance().span();
        match branch.activation() {
            dae::DiscreteBranchActivation::Always => {
                unconditional = Some(lower_discrete_value_branch(
                    context.lowerer,
                    context.shared_calls,
                    &classified,
                    branch,
                )?);
            }
            dae::DiscreteBranchActivation::When { trigger, guard } => {
                require_periodic_trigger(context.view, trigger, context.clock, branch_span)?;
                collect_condition_current_reads(context.view, trigger, &mut reads);
                collect_condition_current_reads(context.view, guard, &mut reads);
                let condition = lower_action_guard(
                    &mut ActionGuardContext {
                        view: context.view,
                        expected: context.clock,
                        lowerer: context.lowerer,
                        span: branch_span,
                    },
                    guard,
                )?
                .unwrap_or(gast::Expression::Bool(true));
                let condition_prefix = context
                    .lowerer
                    .take_prefix_statements_with_shared_calls(&context.shared_calls.entry);
                let body = lower_discrete_value_branch(
                    context.lowerer,
                    context.shared_calls,
                    &classified,
                    branch,
                )?;
                conditional.push(GuardedDiscreteValueBranch {
                    condition_prefix,
                    branch: gast::IfBranch {
                        condition: gast::Condition::Expression(condition),
                        body,
                        span: branch_span,
                    },
                });
            }
        }
        for (value, _) in branch.values().iter() {
            collect_current_reads(context.view, value, &mut reads);
        }
    }
    let statements = compose_discrete_value_branches(conditional, unconditional, span);
    // A group that took a scheduled shared call's result temporaries reads the
    // node that wrote them, so the scheduler keeps that node ahead of it.
    let consumed = context.lowerer.take_consumed_scheduled_calls();
    reads.extend(
        context
            .shared_calls
            .consumed_reads(&consumed)
            .collect::<Vec<_>>(),
    );
    Ok(ClockedAssignment {
        targets: target_variables
            .into_iter()
            .map(|target| target.index())
            .collect(),
        reads,
        statements,
        span,
        is_preamble: false,
        requires_preamble: !context.shared_calls.entry.is_empty(),
    })
}

struct GuardedDiscreteValueBranch {
    condition_prefix: Vec<gast::Spanned<gast::Statement>>,
    branch: gast::IfBranch,
}

/// Preserve lazy `elsewhen` guard evaluation when a guard needs statements.
///
/// A materialized conditional or function call in an `elsewhen` guard must be
/// initialized before that guard is read, but only after every preceding guard
/// has evaluated false. A flat GALEC `elseif` chain cannot place statements
/// between guards, so such branches become nested `else if` statements.
fn compose_discrete_value_branches(
    conditional: Vec<GuardedDiscreteValueBranch>,
    unconditional: Option<Vec<gast::Spanned<gast::Statement>>>,
    span: Span,
) -> Vec<gast::Spanned<gast::Statement>> {
    if conditional.is_empty() {
        return unconditional.unwrap_or_default();
    }
    if conditional
        .iter()
        .all(|branch| branch.condition_prefix.is_empty())
    {
        return vec![gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: conditional
                    .into_iter()
                    .map(|branch| branch.branch)
                    .collect(),
                else_body: unconditional,
            }),
            span,
        )];
    }
    let mut tail = unconditional.unwrap_or_default();
    for guarded in conditional.into_iter().rev() {
        let mut statements = guarded.condition_prefix;
        statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![guarded.branch],
                else_body: (!tail.is_empty()).then_some(tail),
            }),
            span,
        ));
        tail = statements;
    }
    tail
}

fn lower_discrete_value_branch<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    shared_calls: &SharedClockCalls<'dae>,
    targets: &[&ClassifiedVariable<'dae>],
    branch: dae::DiscreteValueBranchView<'dae>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let mut assignments = Vec::new();
    for (classified, (value, provenance)) in targets.iter().zip(branch.values().iter()) {
        append_definition_assignments(
            lowerer,
            value,
            classified,
            provenance.span(),
            &mut assignments,
        )?;
    }
    let mut statements = lowerer.take_prefix_statements_with_shared_calls(&shared_calls.entry);
    statements.extend(assignments);
    Ok(statements)
}

fn collect_current_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    reads: &mut HashSet<u32>,
) {
    dae::for_each_expression(view, expression, |_, node| {
        let id = match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Input(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteValue(id)) => {
                Some(dae::VariableId::from(id))
            }
            _ => None,
        };
        if let Some(id) = id {
            reads.insert(id.index());
        }
    });
}

fn collect_condition_current_reads<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    reads: &mut HashSet<u32>,
) {
    let mut pending = vec![root];
    let mut seen = HashSet::new();
    while let Some(condition) = pending.pop() {
        if !seen.insert(condition.index()) {
            continue;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Clock(_) => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                collect_current_reads(view, expression, reads);
            }
            dae::ConditionOperation::Discrete(expression) => {
                collect_current_reads(view, expression, reads);
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.extend([lhs, rhs]);
            }
        }
    }
}

fn require_periodic_trigger<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    span: Span,
) -> Result<(), GalecTargetError> {
    let mut seen = HashSet::new();
    if condition_requires_clock(view, root, expected, &mut seen) {
        return Ok(());
    }
    Err(unsupported(
        "runtime-event-trigger",
        "a conditional assignment is not owned by the admitted periodic DoStep clock".to_owned(),
        span,
    ))
}

fn condition_requires_clock<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    seen: &mut HashSet<u32>,
) -> bool {
    if !seen.insert(condition.index()) {
        return false;
    }
    match view
        .condition(condition)
        .expect("checked condition identity resolves")
        .operation()
    {
        dae::ConditionOperation::Clock(found) => found == expected,
        dae::ConditionOperation::And(lhs, rhs) => {
            let mut lhs_seen = seen.clone();
            let mut rhs_seen = seen.clone();
            condition_requires_clock(view, lhs, expected, &mut lhs_seen)
                || condition_requires_clock(view, rhs, expected, &mut rhs_seen)
        }
        // Every arm of a disjunction — and every element of a vector activation
        // — must be owned by the clock, or the activation can reach the
        // assignment off-tick.
        dae::ConditionOperation::Or(lhs, rhs) | dae::ConditionOperation::AnyRise(lhs, rhs) => {
            let mut lhs_seen = seen.clone();
            let mut rhs_seen = seen.clone();
            condition_requires_clock(view, lhs, expected, &mut lhs_seen)
                && condition_requires_clock(view, rhs, expected, &mut rhs_seen)
        }
        dae::ConditionOperation::Initial
        | dae::ConditionOperation::Always
        | dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_)
        | dae::ConditionOperation::Not(_) => false,
    }
}

#[cfg(test)]
fn order_assignments(
    pending: &[ClockedAssignment],
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let preamble = pending.iter().position(|assignment| assignment.is_preamble);
    let targets = pending
        .iter()
        .enumerate()
        .flat_map(|(index, assignment)| {
            assignment
                .targets
                .iter()
                .map(move |target| (*target, index))
        })
        .collect::<HashMap<_, _>>();
    let mut emitted = vec![false; pending.len()];
    let mut emitted_owners = 0usize;
    let mut ordered = Vec::with_capacity(pending.len());
    while emitted_owners < pending.len() {
        let Some(index) = pending.iter().enumerate().position(|(index, assignment)| {
            !emitted[index]
                && (!assignment.requires_preamble
                    || preamble.is_none_or(|preamble| emitted[preamble]))
                && assignment.reads.iter().all(|read| {
                    targets
                        .get(read)
                        .is_none_or(|dependency| *dependency == index || emitted[*dependency])
                })
        }) else {
            let span = pending
                .iter()
                .enumerate()
                .find(|(index, _)| !emitted[*index])
                .expect("unfinished ordering has one unemitted owner")
                .1
                .span;
            return Err(unsupported(
                "discrete-algebraic-loop",
                "clocked assignments contain a current-tick dependency cycle".to_owned(),
                span,
            ));
        };
        emitted[index] = true;
        emitted_owners += 1;
        ordered.extend(pending[index].statements.iter().cloned());
    }
    Ok(ordered)
}
