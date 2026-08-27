//! Which repeated calls one clock domain materializes once, and where.
//!
//! A call named once in the model can be reached by several of the domain's
//! values, and the `DoStep` lowering would then evaluate it once per value.
//! This module decides which of those calls are shared, builds the schedule
//! node that evaluates each shared call, and states the guard that keeps a
//! shared call running on exactly the ticks it already ran on. The consumer
//! side stays in the parent module: this one only plans and emits the nodes.

use super::*;

/// The activation a clock-domain value is evaluated under.
///
/// `Entry` is a value the domain clock alone selects: `Always`, or a `when`
/// whose guard names nothing but that clock. Everything else carries the
/// runtime guard it ran under, and may only be shared with values carrying the
/// identical one.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum ValueActivation<'dae> {
    Entry,
    Guarded {
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
    },
}

/// One clock domain's shared eager-call materialization.
pub(super) struct SharedClockCalls<'dae> {
    /// Result temporaries a consumer selected by the domain clock alone may
    /// restore: the domain-entry preamble's and the unguarded scheduled
    /// node's.
    pub(super) entry: SharedMaterializedFunctionCalls,
    /// Result temporaries per guarded activation, readable only by a consumer
    /// carrying that identical activation.
    pub(super) guarded: HashMap<ValueActivation<'dae>, SharedMaterializedFunctionCalls>,
    /// The nodes whose memo entries `entry` carries, in emission order.
    ///
    /// A node built later starts from that memo, so a nested call in its own
    /// argument tree can take an earlier node's result temporary. Declaring a
    /// read of every node it inherits keeps the scheduler from placing it
    /// first: where that would be the only admissible order the graph is
    /// cyclic, ordering fails, and the schedule re-lowers without sharing.
    inherited: Vec<u32>,
    /// Whether any scheduled node was emitted at all.
    pub(super) scheduled: bool,
}

impl<'dae> SharedClockCalls<'dae> {
    /// The result temporaries a group running under `activation` may restore.
    ///
    /// A guarded node's temporaries are written under its own guard, so only a
    /// group carrying that identical activation is proven to run where they
    /// hold. Every other group sees the entry set alone.
    pub(super) fn calls_for(
        &self,
        activation: ValueActivation<'dae>,
    ) -> &SharedMaterializedFunctionCalls {
        self.guarded.get(&activation).unwrap_or(&self.entry)
    }

    /// The schedule edges a group that took `consumed` nodes has to declare.
    ///
    /// The lowerer records the node that wrote each entry it hands out, so this
    /// is already the exact set: a group is ordered after the nodes whose
    /// temporaries it reads and after nothing else.
    pub(super) fn consumed_reads(consumed: &HashSet<u32>) -> impl Iterator<Item = u32> + '_ {
        consumed.iter().copied()
    }
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
///
/// Every shared-call node this domain emitted is checked, which is why the
/// nodes are found by their synthetic targets rather than from a map: one call
/// owner can be materialized by two nodes under two different activations, and
/// both have to hold.
pub(super) fn scheduled_sharing_preserves_arguments(
    pending: &[ClockedAssignment],
    floor: u32,
) -> bool {
    let nodes = pending
        .iter()
        .filter(|assignment| !assignment.is_preamble)
        .filter(|assignment| assignment.targets.iter().any(|target| *target >= floor))
        .collect::<Vec<_>>();
    nodes.iter().all(|node| {
        let ids = node.targets.iter().copied().collect::<HashSet<_>>();
        !pending
            .iter()
            .filter(|assignment| !assignment.reads.is_disjoint(&ids))
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
pub(super) fn lower_clock_domain_shared_calls<'a, 'b, 'dae>(
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
        inherited: Vec::new(),
        scheduled: false,
    };
    let mut slots = ScheduleIdAllocator::new(view, clock);
    let has_preamble = materialize_shared_call_group(
        lowerer,
        pending,
        &preamble_calls,
        SharedCallPlacement::Preamble,
    )?;
    if allow_scheduled && let Some(id) = slots.next() {
        // Built on a clone: sharing is never a reason to refuse a model, so a
        // lowering error declines the node instead of aborting the compile.
        let mut trial = lowerer.clone();
        let before = trial.shared_materialized_function_calls();
        let mut node = Vec::new();
        if matches!(
            materialize_shared_call_group(
                &mut trial,
                &mut node,
                &unguarded_calls,
                SharedCallPlacement::Scheduled {
                    id,
                    requires_preamble: has_preamble,
                },
            ),
            Ok(true)
        ) {
            *lowerer = trial;
            lowerer.register_shared_call_node(&before, id);
            pending.append(&mut node);
            shared.inherited.push(id);
            shared.scheduled = true;
        }
    }
    if allow_scheduled {
        let mut context = SharedCallContext {
            view,
            clock,
            causal,
            lowerer,
            pending,
            slots: &mut slots,
            has_preamble,
            entry_owners,
        };
        lower_conditional_branch_shared_calls(&mut context, &roots, &mut shared);
        shared.entry = context.lowerer.shared_materialized_function_calls();
        lower_guarded_shared_calls(&mut context, &roots, &mut shared);
    } else {
        shared.entry = lowerer.shared_materialized_function_calls();
    }
    Ok(shared)
}

/// One conditional's first branch, and the calls this domain evaluates more
/// than once inside it.
struct ConditionalBranchShare<'dae> {
    /// The ordered condition identities the activation key is built from. Two
    /// conditionals selecting on the same conditions carry the same key, which
    /// is what lets one node serve every value that branches on them.
    operands: Vec<u32>,
    condition: dae::ExprId<'dae>,
    calls: Vec<RepeatedCall<'dae>>,
    span: Span,
}

/// Materialize each call repeated inside one conditional branch, under that
/// branch's own condition.
///
/// The branches of a conditional own separate dynamic executions, so a call
/// inside one runs only where that branch is selected. The node re-states the
/// branch condition around its materialization, so the call keeps exactly that
/// tick set, and lowers under the same
/// [`ConditionalActivationKey`] the branch pushes, so the memo entry it leaves
/// records the branch it holds under. `MaterializedFunctionCallKey::dominates`
/// is then the whole admission proof for a consumer: a group that reaches the
/// call under the same conditions and the same branch matches the entry, and a
/// group under a different branch, a different conditional or none matches
/// nothing and lowers the call for itself.
///
/// Only the first branch is admitted. A later branch runs under the negation of
/// every earlier condition as well, which the node would have to re-state as a
/// nested chain; declining keeps the emitted guard exactly one condition, the
/// one the key names.
///
/// Only values the domain clock alone selects are scanned. A value under a
/// runtime guard evaluates its conditional only where that guard holds, and a
/// node stating the branch condition alone would run on more ticks than the
/// model does.
fn lower_conditional_branch_shared_calls<'dae>(
    context: &mut SharedCallContext<'_, '_, '_, 'dae>,
    roots: &[DomainValueRoot<'dae>],
    shared: &mut SharedClockCalls<'dae>,
) {
    let (view, causal) = (context.view, context.causal);
    let branches = conditional_branch_shares(view, causal, roots, &context.entry_owners);
    for branch in branches {
        let Some(id) = context.slots.next() else {
            return;
        };
        // The node is built on a clone, so a branch that turns out not to lower
        // leaves no temporary, cache entry or emitted statement behind. A
        // lowering error declines the sharing rather than refusing the model.
        let mut trial = context.lowerer.clone();
        let before = trial.shared_materialized_function_calls();
        trial.begin_shared_call_group(&before);
        trial.take_consumed_scheduled_calls();
        let Ok(Some(node)) = lower_conditional_branch_node(&mut trial, &branch) else {
            continue;
        };
        let mut reads = branch.calls.iter().fold(HashSet::new(), |mut reads, call| {
            reads.extend(call.reads.iter().copied());
            reads
        });
        let mut condition_reads = HashSet::new();
        collect_current_reads(view, branch.condition, &mut condition_reads);
        reads.extend(causal.expand(condition_reads));
        // Every node whose memo this one started from, plus every node it was
        // recorded as actually taking a temporary from.
        reads.extend(shared.inherited.iter().copied());
        reads.extend(trial.take_consumed_scheduled_calls());
        *context.lowerer = trial;
        context.lowerer.register_shared_call_node(&before, id);
        context.pending.push(ClockedAssignment {
            targets: HashSet::from([id]),
            reads,
            statements: node,
            span: branch.span,
            is_preamble: false,
            requires_preamble: context.has_preamble,
        });
        shared.inherited.push(id);
        shared.scheduled = true;
    }
}

/// Emit one conditional-branch node's statements, or decline.
///
/// The condition is lowered with the branch's activation key already pushed,
/// which is the order `lower_materialized_conditional_branch` lowers it in, so
/// a call inside the condition matches the same facts on both sides.
/// `conditional_depth` is raised for the same reason: it disables the scalar
/// projection and function-value caches, and the node has to be lowered in the
/// environment its consumers lower in.
fn lower_conditional_branch_node<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    branch: &ConditionalBranchShare<'dae>,
) -> Result<Option<Vec<gast::Spanned<gast::Statement>>>, GalecTargetError> {
    lowerer.conditional_depth += 1;
    lowerer
        .conditional_activation_path
        .push(ConditionalActivationKey {
            kind: ConditionalActivationKind::ConditionalScalar,
            operands: branch.operands.clone(),
            branch: 0,
        });
    let lowered = lower_conditional_branch_body(lowerer, branch);
    lowerer.conditional_activation_path.pop();
    lowerer.conditional_depth -= 1;
    lowered
}

fn lower_conditional_branch_body<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    branch: &ConditionalBranchShare<'dae>,
) -> Result<Option<Vec<gast::Spanned<gast::Statement>>>, GalecTargetError> {
    let condition = lowerer.lower(branch.condition)?;
    require_boolean(&condition, branch.span)?;
    let mut statements = lowerer.drain_prefix_statements();
    for call in &branch.calls {
        lowerer.materialize_eager_call(call.call)?;
    }
    let body = lowerer.drain_prefix_statements();
    if body.is_empty() {
        return Ok(None);
    }
    statements.push(gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(condition.expression),
                body,
                span: branch.span,
            }],
            else_body: None,
        }),
        branch.span,
    ));
    Ok(Some(statements))
}

/// Group the domain's clock-selected values by the conditional branch they
/// evaluate calls in, keeping the calls more than one value reaches.
///
/// A call already materialized at domain entry or by the unguarded node is left
/// out, so nothing is materialized twice. A call that appears under two
/// different branch keys is left out as well: one node states one condition,
/// and the memo it leaves records the call owner rather than the branch.
fn conditional_branch_shares<'a, 'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalReadExpansion<'a, 'dae>,
    roots: &[DomainValueRoot<'dae>],
    entry_owners: &HashSet<u32>,
) -> Vec<ConditionalBranchShare<'dae>> {
    let mut order = Vec::new();
    let mut branches =
        HashMap::<Vec<u32>, (dae::ExprId<'dae>, HashMap<u32, (dae::ExprId<'dae>, usize)>)>::new();
    for root in roots
        .iter()
        .filter(|root| root.activation == ValueActivation::Entry)
    {
        let mut selected = Vec::new();
        let mut seen = HashSet::new();
        expression_functions::for_each_eager_call(
            view,
            root.value,
            &mut seen,
            &|coordinate| causal.inlined_definition(coordinate),
            &mut |site| {
                if let expression_functions::EagerSite::Conditional { operands } = site
                    && operands.len() >= 3
                    && let (Some(condition), Some(value)) = (operands.get(0), operands.get(1))
                {
                    selected.push((conditional_activation_operands(operands), condition, value));
                }
            },
        );
        for (key, condition, value) in selected {
            let entry = branches
                .entry(key.clone())
                .or_insert_with(|| (condition, HashMap::new()));
            let mut branch_seen = seen.clone();
            let mut occurrences = HashMap::new();
            collect_eager_calls(view, value, causal, &mut branch_seen, &mut occurrences);
            for (owner, (call, _)) in occurrences {
                entry
                    .1
                    .entry(owner)
                    .and_modify(|(_, count)| *count += 1)
                    .or_insert((call, 1));
            }
            if !order.contains(&key) {
                order.push(key);
            }
        }
    }
    let mut single_key = HashMap::<u32, usize>::new();
    for key in &order {
        for owner in branches[key].1.keys() {
            *single_key.entry(*owner).or_default() += 1;
        }
    }
    order
        .into_iter()
        .filter_map(|key| {
            let (condition, owners) = branches.remove(&key)?;
            let mut calls = owners
                .into_iter()
                .filter(|(owner, (_, count))| {
                    *count >= 2
                        && !entry_owners.contains(owner)
                        && single_key.get(owner) == Some(&1)
                })
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
            let span = calls.first()?.span;
            Some(ConditionalBranchShare {
                operands: key,
                condition,
                calls,
                span,
            })
        })
        .collect()
}

/// Everything one step of the shared-call plan works against.
struct SharedCallContext<'ctx, 'a, 'b, 'dae> {
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
    context: &mut SharedCallContext<'_, '_, '_, 'dae>,
    roots: &[DomainValueRoot<'dae>],
    shared: &mut SharedClockCalls<'dae>,
) {
    let activations = guarded_activations(roots);
    let selections = conditional_branch_selections(context.view, context.causal, roots);
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
        trial.take_consumed_scheduled_calls();
        let Ok(Some(lowered)) = lower_shared_call_guard(
            context.view,
            context.clock,
            context.causal,
            &mut trial,
            trigger,
            guard,
            span,
        ) else {
            continue;
        };
        let mut reads = lowered.reads;
        let mut declined = false;
        for call in &calls {
            if trial.materialize_eager_call(call.call).is_err() {
                declined = true;
                break;
            }
            reads.extend(call.reads.iter().copied());
        }
        if declined {
            continue;
        }
        let body = trial.drain_prefix_statements();
        if body.is_empty() {
            continue;
        }
        // Every node whose memo this one started from, plus every node it was
        // recorded as actually taking a temporary from.
        reads.extend(shared.inherited.iter().copied());
        reads.extend(trial.take_consumed_scheduled_calls());
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
        context.lowerer.register_shared_call_node(&shared.entry, id);
        let added = context.lowerer.shared_call_entries_added(&shared.entry);
        alias_guarded_node_into_branches(
            context,
            &selections,
            &added,
            guard,
            id,
            &mut shared.entry,
        );
        context.pending.push(ClockedAssignment {
            targets: HashSet::from([id]),
            reads,
            statements,
            span,
            is_preamble: false,
            requires_preamble: context.has_preamble,
        });
        shared.scheduled = true;
    }
}

/// One conditional branch a value of this domain evaluates, and the call
/// owners its own branch conditions evaluate.
struct ConditionalBranchSelection {
    key: ConditionalActivationKey,
    condition_owners: HashSet<u32>,
}

/// Offer one guarded node's result temporaries inside the conditional branch
/// its guard selects.
///
/// An `if`/`elseif` chain in a clocked algorithm becomes two things in the
/// checked DAE: one guarded definition per branch, and one n-ary conditional
/// per value that reads what the chain assigned. The guarded node already
/// evaluates a repeated call once for the definitions, but the conditional's
/// branch evaluates it again, because a memo entry is admitted only where the
/// guard facts it was emitted under hold again and a guarded node states its
/// guard as an emitted `if` rather than as a fact.
///
/// Where the node's guard IS one branch's selection the fact does exist, and it
/// is exactly that branch: [`guard_selects_branch`] proves the guard is the
/// conjunction of the identical condition expressions the conditional selects
/// on, so the node ran on precisely the ticks a use inside that branch is
/// reached on. Its entries are therefore published a second time under that
/// branch's activation key, which `MaterializedFunctionCallKey::dominates`
/// admits inside the branch and nowhere else.
///
/// Three cases decline, because declining costs one repeated call and being
/// wrong costs a call evaluated on a tick the model never evaluates it on:
/// an entry that already carries a guard fact of its own, a branch whose own
/// conditions evaluate the same call owner (the branch key is pushed for the
/// condition too, and the condition runs before its branch is selected), and a
/// key some earlier node already published.
fn alias_guarded_node_into_branches<'dae>(
    context: &mut SharedCallContext<'_, '_, '_, 'dae>,
    selections: &[ConditionalBranchSelection],
    entries: &[(MaterializedFunctionCallKey, Vec<gast::Name>)],
    guard: dae::ConditionId<'dae>,
    node: u32,
    entry: &mut SharedMaterializedFunctionCalls,
) {
    let selected = selections.iter().filter(|selection| {
        guard_selects_branch(
            context.view,
            guard,
            context.clock,
            &selection.key.operands,
            selection.key.branch,
        )
    });
    for selection in selected {
        for (key, names) in entries {
            if !key.activation_path.is_empty() || selection.condition_owners.contains(&key.owner) {
                continue;
            }
            let alias = MaterializedFunctionCallKey {
                activation_path: vec![selection.key.clone()],
                ..key.clone()
            };
            if entry.contains_key(&alias) {
                continue;
            }
            entry.insert(alias.clone(), names.clone());
            context.lowerer.register_shared_call_alias(alias, node);
        }
    }
}

/// Every conditional branch this domain's values evaluate, once per distinct
/// conditional-and-branch pair.
///
/// The branch index is the one `lower_materialized_conditional_branch` pushes:
/// one per condition, plus one for the fallback the chain ends with.
fn conditional_branch_selections<'a, 'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalReadExpansion<'a, 'dae>,
    roots: &[DomainValueRoot<'dae>],
) -> Vec<ConditionalBranchSelection> {
    let mut selections = Vec::new();
    let mut seen_keys = HashSet::new();
    for root in roots {
        let mut seen = HashSet::new();
        let mut conditionals = Vec::new();
        expression_functions::for_each_eager_call(
            view,
            root.value,
            &mut seen,
            &|coordinate| causal.inlined_definition(coordinate),
            &mut |site| {
                if let expression_functions::EagerSite::Conditional { operands } = site {
                    conditionals.push(operands);
                }
            },
        );
        let unseen = conditionals
            .into_iter()
            .filter(|operands| seen_keys.insert(conditional_activation_operands(*operands)));
        for operands in unseen {
            selections.extend(one_conditional_branch_selections(view, causal, operands));
        }
    }
    selections
}

/// The branch selections of one conditional, with the call owners its own
/// conditions evaluate.
fn one_conditional_branch_selections<'a, 'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalReadExpansion<'a, 'dae>,
    operands: dae::ExpressionOperands<'dae>,
) -> Vec<ConditionalBranchSelection> {
    let conditions = conditional_activation_operands(operands);
    let mut condition_owners = HashSet::new();
    for condition in operands.iter().step_by(2).take(conditions.len()) {
        let mut occurrences = HashMap::new();
        let mut seen = HashSet::new();
        collect_eager_calls(view, condition, causal, &mut seen, &mut occurrences);
        condition_owners.extend(occurrences.into_keys());
    }
    (0..=conditions.len())
        .map_while(|branch| u32::try_from(branch).ok())
        .map(|branch| ConditionalBranchSelection {
            key: ConditionalActivationKey {
                kind: ConditionalActivationKind::ConditionalScalar,
                operands: conditions.clone(),
                branch,
            },
            condition_owners: condition_owners.clone(),
        })
        .collect()
}

/// Whether `guard` is exactly the selection of one conditional branch.
///
/// Branch `k` of an n-ary conditional is reached when its own condition holds
/// and every earlier one does not; the fallback branch is reached when none
/// holds. The DAE builds a clocked algorithm's branch guards from the identical
/// condition expressions, so the proof here is a comparison of literal sets
/// rather than a decision procedure: the guard has to be a conjunction of
/// discrete predicates over those expressions, together with this domain's own
/// clock, which holds throughout the `DoStep`, and its positive and negative
/// literals have to be exactly the branch's.
fn guard_selects_branch<'dae>(
    view: dae::DaeView<'dae>,
    guard: dae::ConditionId<'dae>,
    clock: dae::ClockId<'dae>,
    conditions: &[u32],
    branch: u32,
) -> bool {
    let Ok(branch) = usize::try_from(branch) else {
        return false;
    };
    if branch > conditions.len() {
        return false;
    }
    let mut positive = HashSet::new();
    let mut negative = HashSet::new();
    if !collect_guard_literals(view, guard, false, clock, &mut positive, &mut negative) {
        return false;
    }
    let expected_positive = conditions
        .get(branch)
        .copied()
        .into_iter()
        .collect::<HashSet<_>>();
    let expected_negative = conditions[..branch].iter().copied().collect::<HashSet<_>>();
    positive == expected_positive && negative == expected_negative
}

/// Split one guard into the discrete predicates it requires true and false, or
/// decline.
///
/// Only the shapes a branch selection is built from are admitted: conjunction,
/// negation, the disjunction a negated earlier-branch list becomes, this
/// domain's own clock, and a predicate. Anything else, another domain's clock
/// and `initial` included, leaves the guard unclassified and declines.
///
/// A relation names the expression it was interned from, which is the same
/// expression identity a conditional selects on. It is a literal here and not
/// an edge because the activation it belongs to is triggered by this domain's
/// clock: [`lower_action_guard`] lowers it as the level test both the node and
/// its consumer groups already state.
fn collect_guard_literals<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    negated: bool,
    clock: dae::ClockId<'dae>,
    positive: &mut HashSet<u32>,
    negative: &mut HashSet<u32>,
) -> bool {
    // Every lookup here declines rather than asserts: a guard this proof cannot
    // resolve is simply one it cannot classify, and declining costs one
    // repeated call.
    let Some(view_condition) = view.condition(condition) else {
        return false;
    };
    match view_condition.operation() {
        dae::ConditionOperation::Clock(found) => !negated && found == clock,
        dae::ConditionOperation::Relation(relation) => {
            let Some(expression) = view.relation(relation).map(dae::RelationView::expression)
            else {
                return false;
            };
            if negated {
                negative.insert(expression.index());
            } else {
                positive.insert(expression.index());
            }
            true
        }
        dae::ConditionOperation::Discrete(expression) => {
            if negated {
                negative.insert(expression.index());
            } else {
                positive.insert(expression.index());
            }
            true
        }
        dae::ConditionOperation::Not(inner) => {
            collect_guard_literals(view, inner, !negated, clock, positive, negative)
        }
        dae::ConditionOperation::And(lhs, rhs) if !negated => {
            collect_guard_literals(view, lhs, false, clock, positive, negative)
                && collect_guard_literals(view, rhs, false, clock, positive, negative)
        }
        dae::ConditionOperation::Or(lhs, rhs) if negated => {
            collect_guard_literals(view, lhs, true, clock, positive, negative)
                && collect_guard_literals(view, rhs, true, clock, positive, negative)
        }
        _ => false,
    }
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
/// The first index a shared-call node's synthetic target can take.
///
/// Checked DAE variable indices are dense from zero, so every index at or above
/// the checked variable count is one [`ScheduleIdAllocator`] handed out.
pub(super) fn synthetic_schedule_floor(view: dae::DaeView<'_>) -> u32 {
    u32::try_from(view.variable_count()).unwrap_or(u32::MAX)
}

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
        // An `Always` branch is unconditional only when it is the whole owner.
        // `compose_discrete_value_branches` emits it as the `else_body` of the
        // when-chain as soon as the owner has one conditional branch, so it
        // then runs on strictly fewer ticks than the domain does and a call it
        // names must not be hoisted anywhere unguarded.
        let has_conditional_branch = owner.branches().iter().any(|branch| {
            matches!(
                branch.activation(),
                dae::DiscreteBranchActivation::When { .. }
            )
        });
        for branch in owner.branches().iter() {
            let selected_by_clock = match branch.activation() {
                dae::DiscreteBranchActivation::Always => !has_conditional_branch,
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
pub(super) fn value_activation<'dae>(
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
        &mut |site| {
            // Count construction-issued invocations, not scalar projections.
            if let expression_functions::EagerSite::Call { call, owner } = site {
                occurrences
                    .entry(owner)
                    .and_modify(|(_, count)| *count += 1)
                    .or_insert((call, 1));
            }
        },
    );
}
