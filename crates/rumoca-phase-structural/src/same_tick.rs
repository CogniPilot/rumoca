//! Construction-issued same-tick producer ordering for clocked discrete owners.
//!
//! This is the structural half of the SPEC_0040 `SOLVE-C57`
//! `ClockPartitionTransactionProgram` design
//! (SPEC_0046 §2/§6 step 3),
//! implemented toward the SPEC_0046 SDO-001/SDO-002 semantics: inside one
//! event/clock tick an ordinary same-instant read consumes this tick's value
//! (`next`), and only an explicit `pre`, `previous`, or `sample(u)` consumes
//! its named history lane.
//!
//! The caller (Solve lowering) describes each clocked producer — its complete
//! target tuple and the expressions and conditions it evaluates at the tick —
//! and this module *issues* the executable order: a topological schedule of
//! producers by their same-instant reads, alias-followed through the exact
//! definitions of [`SameTickDefinitions`]. Rank is the issued position; no
//! consumer recovers it by searching an order list or by matching targets,
//! names, spans, or provenance.
//!
//! An algebraic intermediate on a same-tick path between two producers (an
//! alias chain such as `slow = fastAlias; fastAlias = fast`) is issued as its
//! own [`SameTickStep::IntermediateDefinition`] step, ordered after the
//! producers it reads and before the producers that read it. The runtime
//! refreshes it into private work state so later producers observe this tick's
//! value; it is never committed by the discrete pass ("intermediates become
//! visible to later producers without committing unrelated targets").
//!
//! [`SameTickDefinitions`] is deliberately wider than [`CausalDefinitions`]:
//! the proven acyclic algebraic elimination only orients whole *algebraic*
//! coordinates whose value type matches exactly, so `Real nAlias = n` with an
//! `Integer` producer `n`, `Integer nAlias = n`, and a connector-routed
//! `Integer` port all fall outside it. Those are precisely the RDD2/eFMI
//! mode/status-code shapes, so this module derives its own exact
//! single-writer definitions for them (SPEC_0046 SDO-002) and *fails
//! construction* rather than assuming a read whose producer-reachable owner it
//! cannot refresh is already current at tick entry.
//!
//! A directed cycle among same-tick reads with no `pre`/`previous` boundary is
//! a construction-time rejection at the blocked producer's span (the MLS
//! Appendix B discrete algebraic-loop rule), never a runtime fallback.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_core::Span;
use rumoca_ir_dae as dae;

use crate::CausalDefinitions;

/// One clocked producer described by the Solve construction.
///
/// `value_reads` and `condition_reads` are the expression and condition roots
/// the producer evaluates at its tick. A producer whose reads all resolve
/// through a history lane (for example a `sample(u)` source) passes no roots.
pub struct SameTickProducer<'dae> {
    /// Canonical periodic-clock domains on which this producer can execute.
    /// Same-tick exchange exists only between producers with an intersecting
    /// domain; a disjoint-clock read observes held entry storage.
    pub clock_domains: Vec<u32>,
    /// Complete target tuple (DAE variable identities written by the producer).
    pub targets: Vec<dae::VariableId<'dae>>,
    /// Value expression roots evaluated at the tick.
    pub value_reads: Vec<dae::ExprId<'dae>>,
    /// Condition roots (guards/triggers) evaluated at the tick.
    pub condition_reads: Vec<dae::ConditionId<'dae>>,
    /// Producer source span for rejection diagnostics.
    pub span: Span,
}

/// One issued step of the same-tick schedule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SameTickStep<'dae> {
    /// Execute the producer at the given caller index.
    Producer(usize),
    /// Refresh the exact definition of one algebraic intermediate into private
    /// work state. The definition travels with the step: the consumer lowers
    /// exactly the expression this module ordered, never one it re-derives.
    IntermediateDefinition {
        variable: dae::VariableId<'dae>,
        definition: dae::ExprId<'dae>,
    },
}

/// The issued schedule: every producer exactly once, interleaved with the
/// intermediate definitions same-tick consumers need, in causal order.
pub struct SameTickSchedule<'dae> {
    pub steps: Vec<SameTickStep<'dae>>,
    /// Consumer producer indices for every issued intermediate variable.
    ///
    /// This is derived by the same dependency walk that issued `steps`. Solve
    /// lowering maps the consumers to their typed clock domains once; a
    /// runtime must not rediscover this liveness from programs or storage.
    pub intermediate_consumers: BTreeMap<u32, Vec<usize>>,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum SameTickOrderError {
    #[error("same-tick producer clock domains are not a nonempty canonical set")]
    InvalidClockDomains { span: Span },
    #[error(
        "same-tick discrete producers form an algebraic loop with no pre()/previous() boundary: {detail}"
    )]
    Cycle { span: Span, detail: String },
    #[error("variable `{name}` is written by more than one same-tick producer")]
    DuplicateOwner { span: Span, name: String },
    #[error("same-tick read of `{name}` cannot be refreshed inside the clock partition: {detail}")]
    UnrefreshableRead {
        span: Span,
        name: String,
        detail: String,
    },
}

impl SameTickOrderError {
    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            Self::InvalidClockDomains { span }
            | Self::Cycle { span, .. }
            | Self::DuplicateOwner { span, .. }
            | Self::UnrefreshableRead { span, .. } => *span,
        }
    }
}

/// Exact same-tick definitions, derived once per DAE.
///
/// `exact` holds the single unconditional definition of a coordinate that a
/// same-tick consumer can be refreshed from; `opaque` holds the coordinates
/// that *are* written by some owner whose value this module cannot reproduce
/// exactly, together with what those writers read. A coordinate in neither map
/// has no writer at all (a state, parameter, input, or history lane) and is
/// therefore genuinely current at tick entry.
#[derive(Default)]
pub struct SameTickDefinitions<'dae> {
    ids: Vec<Option<dae::VariableId<'dae>>>,
    exact: BTreeMap<u32, dae::ExprId<'dae>>,
    opaque: BTreeMap<u32, BTreeSet<u32>>,
    /// Exact always-active discrete definitions must execute inside a clock
    /// partition when one of its producers reads them. Their stored entry
    /// value predates the tick, even when they do not depend on another
    /// clock-owned producer (for example `fresh = valid and sample(...)`).
    refresh_on_tick: BTreeSet<u32>,
}

/// One coordinate's accumulated writers while [`SameTickDefinitions`] derives.
#[derive(Default)]
struct WriterAccumulator<'dae> {
    count: usize,
    definition: Option<dae::ExprId<'dae>>,
    reads: BTreeSet<u32>,
}

type Writers<'dae> = BTreeMap<u32, WriterAccumulator<'dae>>;

impl<'dae> SameTickDefinitions<'dae> {
    /// Derive the exact-definition map for one branded DAE view.
    ///
    /// [`CausalDefinitions`] is authoritative wherever it applies: it already
    /// proves orientation and acyclicity for whole algebraic aliases. This
    /// derivation adds the exact single-writer definitions that proof declines
    /// (a widening `Real x = integerDiscrete`, a `B.1c` value owner, an exact
    /// generated connection row) and records every other writer as opaque.
    #[must_use]
    pub fn derive(view: dae::DaeView<'dae>, causal: &CausalDefinitions<'dae>) -> Self {
        let mut ids = vec![None; view.variable_count()];
        for (id, _) in view.variables() {
            if let Some(slot) = ids.get_mut(id.index() as usize) {
                *slot = Some(id);
            }
        }
        let mut writers = Writers::new();
        let mut refresh_on_tick = BTreeSet::new();
        collect_continuous_writers(view, &mut writers);
        collect_discrete_real_writers(view, &mut writers, &mut refresh_on_tick);
        collect_discrete_value_writers(view, &mut writers, &mut refresh_on_tick);

        let mut exact = BTreeMap::new();
        let mut opaque = BTreeMap::new();
        for (variable, writer) in writers {
            match writer.definition {
                Some(definition) if writer.count == 1 => {
                    exact.insert(variable, definition);
                }
                _ => {
                    opaque.insert(variable, writer.reads);
                }
            }
        }
        for (id, _) in view.variables() {
            if let Some(definition) = causal.definition_for_variable(id) {
                opaque.remove(&id.index());
                exact.insert(id.index(), definition);
            }
        }
        refresh_on_tick.retain(|variable| exact.contains_key(variable));
        Self {
            ids,
            exact,
            opaque,
            refresh_on_tick,
        }
    }

    /// The exact definition a same-tick consumer refreshes this coordinate from.
    #[must_use]
    pub fn definition(&self, variable: u32) -> Option<dae::ExprId<'dae>> {
        self.exact.get(&variable).copied()
    }

    /// What the writers of a coordinate this module cannot refresh read.
    #[must_use]
    pub fn opaque_reads(&self, variable: u32) -> Option<&BTreeSet<u32>> {
        self.opaque.get(&variable)
    }

    /// The branded identity of one variable index (no linear rescan).
    #[must_use]
    pub fn variable_id(&self, variable: u32) -> Option<dae::VariableId<'dae>> {
        self.ids.get(variable as usize).copied().flatten()
    }

    /// Whether this exact definition owns stored discrete output that is stale
    /// until the current tick evaluates its always-active equation.
    #[must_use]
    pub fn refreshes_on_tick(&self, variable: u32) -> bool {
        self.refresh_on_tick.contains(&variable)
    }

    /// The complete set of coordinates one prospective producer observes at its
    /// tick, alias-followed through the exact definitions.
    ///
    /// Solve lowering uses this to decide *program granularity*: two targets
    /// may share one fused program only when neither can observe the other on
    /// the tick, so restoring fusion never reintroduces a stale same-tick read.
    #[must_use]
    pub fn read_closure(
        &self,
        view: dae::DaeView<'dae>,
        value_reads: &[dae::ExprId<'dae>],
        condition_reads: &[dae::ConditionId<'dae>],
    ) -> BTreeSet<u32> {
        let mut pending = Vec::new();
        let mut direct = BTreeSet::new();
        for &value in value_reads {
            collect_same_instant_reads(view, value, &mut direct);
        }
        for &condition in condition_reads {
            collect_condition_same_instant_reads(view, condition, &mut direct);
        }
        pending.extend(direct.iter().copied());
        let mut closure = BTreeSet::new();
        while let Some(variable) = pending.pop() {
            if !closure.insert(variable) {
                continue;
            }
            if let Some(definition) = self.definition(variable) {
                let mut reads = BTreeSet::new();
                collect_same_instant_reads(view, definition, &mut reads);
                pending.extend(reads);
            } else if let Some(reads) = self.opaque_reads(variable) {
                pending.extend(reads.iter().copied());
            }
        }
        closure
    }
}

fn record_definition<'dae>(
    view: dae::DaeView<'dae>,
    writers: &mut Writers<'dae>,
    target: u32,
    value: dae::ExprId<'dae>,
) {
    let entry = writers.entry(target).or_default();
    entry.count += 1;
    entry.definition = (entry.count == 1).then_some(value);
    collect_same_instant_reads(view, value, &mut entry.reads);
}

fn record_opaque_writer<'dae>(writers: &mut Writers<'dae>, target: u32, reads: &BTreeSet<u32>) {
    let entry = writers.entry(target).or_default();
    entry.count += 1;
    entry.definition = None;
    entry.reads.extend(reads.iter().copied());
}

/// Continuous residual owners contribute a definition only in the exact
/// `algebraic - value` shape. A whole-alias pair (`x - y = 0`) is left to
/// [`CausalDefinitions`], which owns its orientation proof.
fn collect_continuous_writers<'dae>(view: dae::DaeView<'dae>, writers: &mut Writers<'dae>) {
    for owner in view.continuous_owners() {
        let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
            continue;
        };
        if let Some((target, value)) =
            single_coordinate_definition(view, equation.residual(), CoordinateKinds::Algebraic)
        {
            record_definition(view, writers, target, value);
        }
    }
}

/// Always-active discrete Real equations (the generated connection rows) may
/// define a whole algebraic or discrete-Real coordinate.
fn collect_discrete_real_writers<'dae>(
    view: dae::DaeView<'dae>,
    writers: &mut Writers<'dae>,
    refresh_on_tick: &mut BTreeSet<u32>,
) {
    for index in 0..view.discrete_real_equation_count() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        if equation.activation() != dae::DiscreteRealActivation::Always {
            continue;
        }
        if let Some((target, value)) = single_coordinate_definition(
            view,
            equation.residual(),
            CoordinateKinds::AlgebraicOrDiscreteReal,
        ) {
            record_definition(view, writers, target, value);
            refresh_on_tick.insert(target);
        }
    }
}

/// `B.1c` value owners name their targets explicitly, so both their exact
/// unconditional definitions and their unrefreshable conditional forms are
/// recorded without guessing an orientation.
fn collect_discrete_value_writers<'dae>(
    view: dae::DaeView<'dae>,
    writers: &mut Writers<'dae>,
    refresh_on_tick: &mut BTreeSet<u32>,
) {
    for index in 0..view.discrete_value_owner_count() {
        let id = view
            .discrete_value_owner_id(index)
            .expect("dense B.1c owner identity resolves");
        let owner = view
            .discrete_value_owner(id)
            .expect("checked B.1c owner resolves");
        let branches = owner.branches();
        let first = branches.get(0);
        let unconditional = owner.structure().is_none()
            && branches.len() == 1
            && first.is_some_and(|branch| {
                matches!(branch.activation(), dae::DiscreteBranchActivation::Always)
                    && branch.values().len() == owner.targets().len()
            });
        if unconditional {
            let branch = first.expect("checked unconditional B.1c branch resolves");
            for (target, (value, _)) in owner.targets().iter().zip(branch.values().iter()) {
                let target = dae::VariableId::from(target).index();
                record_definition(view, writers, target, value);
                refresh_on_tick.insert(target);
            }
            continue;
        }
        let mut reads = BTreeSet::new();
        for branch in branches.iter() {
            for (value, _) in branch.values().iter() {
                collect_same_instant_reads(view, value, &mut reads);
            }
        }
        for target in owner.targets().iter() {
            record_opaque_writer(writers, dae::VariableId::from(target).index(), &reads);
        }
    }
}

/// Which whole-coordinate kinds may appear as the *target* side of an exact
/// definition residual.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CoordinateKinds {
    Algebraic,
    AlgebraicOrDiscreteReal,
}

impl CoordinateKinds {
    fn admits(self, coordinate: dae::CoordinateView<'_>) -> bool {
        matches!(
            (self, coordinate),
            (_, dae::CoordinateView::Algebraic(_))
                | (
                    Self::AlgebraicOrDiscreteReal,
                    dae::CoordinateView::DiscreteReal(_)
                )
        )
    }
}

/// The exact `target - value` (or `value - target`) definition of one residual.
///
/// Exactly one side must be a whole admissible coordinate; the value side may
/// be any expression, including a whole coordinate of a *different* kind or
/// value type — that widening alias (`Real nAlias = n`) is exactly the shape
/// the causal elimination declines and the one a same-tick reader needs.
fn single_coordinate_definition<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
    kinds: CoordinateKinds,
) -> Option<(u32, dae::ExprId<'dae>)> {
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = view.expression(residual)?.operation()
    else {
        return None;
    };
    let lhs_target = admissible_target(view, lhs, kinds);
    let rhs_target = admissible_target(view, rhs, kinds);
    match (lhs_target, rhs_target) {
        (Some(target), None) => Some((target, rhs)),
        (None, Some(target)) => Some((target, lhs)),
        _ => None,
    }
}

fn admissible_target<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    kinds: CoordinateKinds,
) -> Option<u32> {
    let dae::ExpressionOperation::Coordinate(coordinate) = view.expression(expression)?.operation()
    else {
        return None;
    };
    kinds.admits(coordinate).then(|| match coordinate {
        dae::CoordinateView::Algebraic(id) => dae::VariableId::from(id).index(),
        dae::CoordinateView::DiscreteReal(id) => dae::VariableId::from(id).index(),
        _ => unreachable!("only admitted coordinate kinds reach the target projection"),
    })
}

/// Issue the same-tick execution schedule for one set of clocked producers.
///
/// `excluded` names the targets a DAE-C21/SOLVE-C55 event transaction owns:
/// those commit before the issued order runs, so they are current at tick
/// entry and are never scheduled or refreshed here.
///
/// See the module documentation for the ordering relation. The result is a
/// value the consumer replays; nothing about it is rediscovered at runtime.
pub fn issue_same_tick_schedule<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &SameTickDefinitions<'dae>,
    producers: &[SameTickProducer<'dae>],
    excluded: &BTreeSet<u32>,
) -> Result<SameTickSchedule<'dae>, SameTickOrderError> {
    if producers.is_empty() {
        return Ok(SameTickSchedule {
            steps: Vec::new(),
            intermediate_consumers: BTreeMap::new(),
        });
    }
    if let Some(producer) = producers.iter().find(|producer| {
        producer.clock_domains.is_empty()
            || producer
                .clock_domains
                .windows(2)
                .any(|pair| pair[0] >= pair[1])
    }) {
        return Err(SameTickOrderError::InvalidClockDomains {
            span: producer.span,
        });
    }
    let producer_of = map_producer_targets(view, producers)?;
    let direct_reads = collect_producer_reads(view, producers);
    let intermediates = discover_intermediates(
        view,
        definitions,
        &producer_of,
        excluded,
        &direct_reads,
        producers,
    )?;
    emit_schedule(
        view,
        definitions,
        producers,
        &producer_of,
        &direct_reads,
        &intermediates,
    )
}

/// Map every producer target to its owning producer, rejecting a target two
/// producers claim (the partition would have no single owner to order).
fn map_producer_targets<'dae>(
    view: dae::DaeView<'dae>,
    producers: &[SameTickProducer<'dae>],
) -> Result<BTreeMap<u32, usize>, SameTickOrderError> {
    let mut producer_of = BTreeMap::<u32, usize>::new();
    for (index, producer) in producers.iter().enumerate() {
        for target in &producer.targets {
            if let Some(previous) = producer_of.insert(target.index(), index)
                && previous != index
            {
                return Err(SameTickOrderError::DuplicateOwner {
                    span: producer.span,
                    name: variable_name(view, target.index()),
                });
            }
        }
    }
    Ok(producer_of)
}

/// The same-instant reads of each producer's values and conditions.
fn collect_producer_reads<'dae>(
    view: dae::DaeView<'dae>,
    producers: &[SameTickProducer<'dae>],
) -> Vec<BTreeSet<u32>> {
    producers
        .iter()
        .map(|producer| {
            let mut reads = BTreeSet::new();
            for &value in &producer.value_reads {
                collect_same_instant_reads(view, value, &mut reads);
            }
            for &condition in &producer.condition_reads {
                collect_condition_same_instant_reads(view, condition, &mut reads);
            }
            reads
        })
        .collect()
}

/// One discovered non-producer coordinate on a same-tick path.
struct IntermediateNode<'dae> {
    definition: Option<dae::ExprId<'dae>>,
    reads: BTreeSet<u32>,
    consumers: BTreeSet<usize>,
}

/// Discover the coordinates whose values sit on a same-tick path into a
/// producer target, together with each one's own direct reads.
///
/// Only coordinates that transitively reach a producer target are kept: those
/// are the storage coordinates whose event-entry values go stale mid-tick. A
/// kept coordinate with no exact definition cannot be refreshed, so it fails
/// construction instead of silently degrading to the pre-C57 stale read.
fn discover_intermediates<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &SameTickDefinitions<'dae>,
    producer_of: &BTreeMap<u32, usize>,
    excluded: &BTreeSet<u32>,
    direct_reads: &[BTreeSet<u32>],
    producers: &[SameTickProducer<'dae>],
) -> Result<BTreeMap<u32, IntermediateNode<'dae>>, SameTickOrderError> {
    let mut discovered = BTreeMap::<u32, IntermediateNode<'dae>>::new();
    let mut pending: Vec<(usize, u32)> = direct_reads
        .iter()
        .enumerate()
        .flat_map(|(consumer, reads)| reads.iter().map(move |&read| (consumer, read)))
        .collect();
    let mut visited = BTreeSet::new();
    while let Some((consumer, variable)) = pending.pop() {
        if !visited.insert((consumer, variable))
            || producer_of.contains_key(&variable)
            || excluded.contains(&variable)
        {
            continue;
        }
        let (definition, reads) = if let Some(definition) = definitions.definition(variable) {
            let mut reads = BTreeSet::new();
            collect_same_instant_reads(view, definition, &mut reads);
            (Some(definition), reads)
        } else if let Some(reads) = definitions.opaque_reads(variable) {
            (None, reads.clone())
        } else {
            // No writer at all: a state, parameter, input, or history lane.
            // Its tick-entry value *is* its value for the whole tick.
            continue;
        };
        pending.extend(reads.iter().map(|&read| (consumer, read)));
        let node = discovered
            .entry(variable)
            .or_insert_with(|| IntermediateNode {
                definition,
                reads,
                consumers: BTreeSet::new(),
            });
        node.consumers.insert(consumer);
    }
    let mut reach = BTreeMap::<u32, bool>::new();
    let reaches_producer: BTreeMap<u32, bool> = discovered
        .keys()
        .copied()
        .collect::<Vec<_>>()
        .into_iter()
        .map(|variable| {
            let mut guard = BTreeSet::new();
            let value =
                reaches_producer_target(variable, producer_of, &discovered, &mut reach, &mut guard);
            (variable, value)
        })
        .collect();
    let mut tick_reach = BTreeMap::<u32, bool>::new();
    let reaches_tick_refresh: BTreeMap<u32, bool> = discovered
        .keys()
        .copied()
        .collect::<Vec<_>>()
        .into_iter()
        .map(|variable| {
            let mut guard = BTreeSet::new();
            let value = reaches_tick_refresh_definition(
                variable,
                definitions,
                &discovered,
                &mut tick_reach,
                &mut guard,
            );
            (variable, value)
        })
        .collect();
    discovered.retain(|variable, _| {
        reaches_producer.get(variable).copied().unwrap_or(false)
            || reaches_tick_refresh.get(variable).copied().unwrap_or(false)
    });
    if let Some((variable, _)) = discovered
        .iter()
        .find(|(_, node)| node.definition.is_none())
    {
        return Err(unrefreshable_read(view, producers, direct_reads, *variable));
    }
    Ok(discovered)
}

/// Whether this definition sits between a producer read and an always-active
/// discrete definition whose stored value must be recomputed for this tick.
fn reaches_tick_refresh_definition(
    variable: u32,
    definitions: &SameTickDefinitions<'_>,
    discovered: &BTreeMap<u32, IntermediateNode<'_>>,
    reach: &mut BTreeMap<u32, bool>,
    guard: &mut BTreeSet<u32>,
) -> bool {
    if definitions.refreshes_on_tick(variable) {
        return true;
    }
    if let Some(&known) = reach.get(&variable) {
        return known;
    }
    if !guard.insert(variable) {
        return false;
    }
    let result = discovered.get(&variable).is_some_and(|node| {
        node.reads.iter().any(|&read| {
            reaches_tick_refresh_definition(read, definitions, discovered, reach, guard)
        })
    });
    guard.remove(&variable);
    reach.insert(variable, result);
    result
}

/// Report a read this module cannot refresh, at the span of a producer that
/// observes it (directly when one does, otherwise the first producer).
fn unrefreshable_read<'dae>(
    view: dae::DaeView<'dae>,
    producers: &[SameTickProducer<'dae>],
    direct_reads: &[BTreeSet<u32>],
    variable: u32,
) -> SameTickOrderError {
    let span = producers
        .iter()
        .zip(direct_reads)
        .find_map(|(producer, reads)| reads.contains(&variable).then_some(producer.span))
        .unwrap_or(producers[0].span);
    SameTickOrderError::UnrefreshableRead {
        span,
        name: variable_name(view, variable),
        detail: "it is written by an owner with no exact unconditional definition, yet its value \
             depends on a producer of this clock partition; write the read through \
             pre()/previous() or give the coordinate a single unconditional definition"
            .to_owned(),
    }
}

fn reaches_producer_target(
    variable: u32,
    producer_of: &BTreeMap<u32, usize>,
    discovered: &BTreeMap<u32, IntermediateNode<'_>>,
    reach: &mut BTreeMap<u32, bool>,
    guard: &mut BTreeSet<u32>,
) -> bool {
    if producer_of.contains_key(&variable) {
        return true;
    }
    if let Some(&known) = reach.get(&variable) {
        return known;
    }
    if !guard.insert(variable) {
        return false;
    }
    let result = discovered.get(&variable).is_some_and(|node| {
        node.reads
            .iter()
            .any(|&read| reaches_producer_target(read, producer_of, discovered, reach, guard))
    });
    guard.remove(&variable);
    reach.insert(variable, result);
    result
}

/// Node universe: producers in caller order, then intermediates by variable
/// index. Emission scans this fixed sequence, so the issued order is
/// deterministic and biased toward source order, exactly like the proven GALEC
/// `order_clocked_assignments`.
#[derive(Clone, Copy)]
enum Node {
    Producer(usize),
    Intermediate(u32),
}

/// Emit the topological order, or reject the same-tick cycle that blocks it.
fn emit_schedule<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &SameTickDefinitions<'dae>,
    producers: &[SameTickProducer<'dae>],
    producer_of: &BTreeMap<u32, usize>,
    direct_reads: &[BTreeSet<u32>],
    intermediates: &BTreeMap<u32, IntermediateNode<'dae>>,
) -> Result<SameTickSchedule<'dae>, SameTickOrderError> {
    let graph = ScheduleGraph {
        producers,
        direct_reads,
        intermediates,
        // A read is satisfied when its owning node (producer target or kept
        // intermediate) has been emitted. A read owned by no node reached no
        // producer target during discovery, so its tick-entry value holds for
        // the whole tick; discovery already failed construction for every read
        // that would have needed a refresh it could not issue.
        // The variable -> node index map is built once: no node recovers
        // another node's position by scanning the node sequence.
        node_of_intermediate: intermediates
            .keys()
            .enumerate()
            .map(|(position, &variable)| (variable, producers.len() + position))
            .collect(),
        producer_of,
        nodes: (0..producers.len())
            .map(Node::Producer)
            .chain(intermediates.keys().copied().map(Node::Intermediate))
            .collect(),
    };
    let mut emitted = vec![false; graph.nodes.len()];
    let mut steps = Vec::with_capacity(graph.nodes.len());
    loop {
        let next = graph
            .nodes
            .iter()
            .copied()
            .enumerate()
            .position(|(index, node)| {
                !emitted[index]
                    && graph.node_reads(node).iter().all(|&read| {
                        graph
                            .owner_of(node, read)
                            .is_none_or(|owner| owner == index || emitted[owner])
                    })
            });
        let Some(index) = next else { break };
        emitted[index] = true;
        steps.push(match graph.nodes[index] {
            Node::Producer(producer) => SameTickStep::Producer(producer),
            Node::Intermediate(variable) => SameTickStep::IntermediateDefinition {
                variable: definitions
                    .variable_id(variable)
                    .expect("a discovered intermediate has a DAE variable identity"),
                definition: intermediates[&variable]
                    .definition
                    .expect("a kept intermediate has an exact definition"),
            },
        });
    }
    if steps.len() != graph.nodes.len() {
        return Err(graph.cycle_error(view, &emitted));
    }
    let intermediate_consumers = intermediates
        .iter()
        .map(|(&variable, node)| (variable, node.consumers.iter().copied().collect()))
        .collect();
    Ok(SameTickSchedule {
        steps,
        intermediate_consumers,
    })
}

/// The read adjacency the emission loop and its rejection diagnostic share.
struct ScheduleGraph<'a, 'dae> {
    producers: &'a [SameTickProducer<'dae>],
    direct_reads: &'a [BTreeSet<u32>],
    intermediates: &'a BTreeMap<u32, IntermediateNode<'dae>>,
    node_of_intermediate: BTreeMap<u32, usize>,
    producer_of: &'a BTreeMap<u32, usize>,
    nodes: Vec<Node>,
}

impl ScheduleGraph<'_, '_> {
    fn node_reads(&self, node: Node) -> &BTreeSet<u32> {
        match node {
            Node::Producer(index) => &self.direct_reads[index],
            Node::Intermediate(variable) => &self.intermediates[&variable].reads,
        }
    }

    fn owner_of(&self, consumer: Node, variable: u32) -> Option<usize> {
        if let Some(&producer) = self.producer_of.get(&variable) {
            return self
                .node_overlaps_producer(consumer, producer)
                .then_some(producer);
        }
        self.node_of_intermediate.get(&variable).copied()
    }

    fn node_overlaps_producer(&self, node: Node, producer: usize) -> bool {
        let owner_domains = &self.producers[producer].clock_domains;
        match node {
            Node::Producer(index) => {
                clock_domains_overlap(&self.producers[index].clock_domains, owner_domains)
            }
            Node::Intermediate(variable) => {
                self.intermediates[&variable]
                    .consumers
                    .iter()
                    .any(|&consumer| {
                        clock_domains_overlap(
                            &self.producers[consumer].clock_domains,
                            owner_domains,
                        )
                    })
            }
        }
    }

    /// Report the same-tick cycle that left the schedule unfinished, preferring
    /// a blocked producer: it carries the source span the model author wrote.
    fn cycle_error(&self, view: dae::DaeView<'_>, emitted: &[bool]) -> SameTickOrderError {
        let (blocked_index, blocked_node) = self
            .nodes
            .iter()
            .copied()
            .enumerate()
            .filter(|(index, _)| !emitted[*index])
            .find(|(_, node)| matches!(node, Node::Producer(_)))
            .or_else(|| {
                self.nodes
                    .iter()
                    .copied()
                    .enumerate()
                    .find(|(index, _)| !emitted[*index])
            })
            .expect("an unfinished schedule has an unemitted node");
        let (span, target_names) = match blocked_node {
            Node::Producer(producer) => (
                self.producers[producer].span,
                self.producers[producer]
                    .targets
                    .iter()
                    .map(|target| variable_name(view, target.index()))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Node::Intermediate(variable) => (
                self.producers
                    .iter()
                    .zip(self.direct_reads)
                    .find_map(|(producer, reads)| {
                        reads.contains(&variable).then_some(producer.span)
                    })
                    .unwrap_or(self.producers[0].span),
                variable_name(view, variable),
            ),
        };
        let mut dependencies = self
            .node_reads(blocked_node)
            .iter()
            .filter_map(|&read| {
                self.owner_of(blocked_node, read).and_then(|owner| {
                    (!emitted[owner] && owner != blocked_index)
                        .then(|| format!("`{}`", variable_name(view, read)))
                })
            })
            .collect::<Vec<_>>();
        dependencies.sort();
        dependencies.dedup();
        SameTickOrderError::Cycle {
            span,
            detail: format!(
                "the definition of `{target_names}` reads {} on the same tick; write the reads \
                 through pre()/previous() or restructure the equations (MLS Appendix B solves a \
                 clock partition once per tick and admits no simultaneous discrete loop)",
                dependencies.join(", ")
            ),
        }
    }
}

fn clock_domains_overlap(lhs: &[u32], rhs: &[u32]) -> bool {
    let mut left = 0usize;
    let mut right = 0usize;
    while left < lhs.len() && right < rhs.len() {
        match lhs[left].cmp(&rhs[right]) {
            std::cmp::Ordering::Less => left += 1,
            std::cmp::Ordering::Greater => right += 1,
            std::cmp::Ordering::Equal => return true,
        }
    }
    false
}

fn variable_name(view: dae::DaeView<'_>, variable: u32) -> String {
    view.variables()
        .find(|(id, _)| id.index() == variable)
        .map(|(_, node)| node.name().to_string())
        .unwrap_or_else(|| format!("#{variable}"))
}

/// Collect the variables an expression reads *at the same instant*.
///
/// `pre(..)`, `previous(..)`, and delay coordinates carry values from a
/// strictly earlier instant (SDO-002's exception set) and are excluded; every
/// current-lane coordinate kind is included, mirroring the proven GALEC
/// `collect_current_reads`.
pub fn collect_same_instant_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    reads: &mut BTreeSet<u32>,
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

/// Collect the same-instant reads of one condition DAG.
pub fn collect_condition_same_instant_reads<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    reads: &mut BTreeSet<u32>,
) {
    let mut pending = vec![root];
    let mut seen = BTreeSet::new();
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
                collect_same_instant_reads(view, expression, reads);
            }
            dae::ConditionOperation::Discrete(expression) => {
                collect_same_instant_reads(view, expression, reads);
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

#[cfg(test)]
mod tests {
    use rumoca_core::{SourceMap, Span, TypeId, VarName};

    use super::*;

    /// Which same-tick paths the fixture wires between its coordinates.
    ///
    /// One fixture shape for every test keeps each test's *own* assertion about
    /// ordering, refusal, or closure the only thing that varies, so a
    /// regression cannot hide behind a differently shaped model.
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Wiring {
        /// `aAlias = a` (matching value types, so the causal elimination owns
        /// it) and `nAlias = n` as a `B.1c` value alias of the Integer producer
        /// (which the causal elimination declines entirely).
        Aliases,
        /// The `Aliases` wiring plus a second, conflicting definition of
        /// `ambiguous`: two writers, so no exact definition exists.
        AliasesAndAmbiguousWriter,
    }

    #[derive(Clone, Copy)]
    struct FixtureVariables<'dae> {
        a: dae::DiscreteRealId<'dae>,
        b: dae::DiscreteRealId<'dae>,
        n: dae::DiscreteValueId<'dae>,
        a_alias: dae::AlgebraicId<'dae>,
        n_alias: dae::DiscreteValueId<'dae>,
        ambiguous: dae::AlgebraicId<'dae>,
    }

    struct FixtureRows<'dae> {
        alias: dae::ExprId<'dae>,
        ambiguous: [dae::ExprId<'dae>; 2],
        n_value: dae::ExprId<'dae>,
        n_alias_value: dae::ExprId<'dae>,
    }

    fn define_fixture_variables<'dae>(
        dae: &mut dae::DaeConstruction<'dae>,
        at: dae::DaeProvenance,
    ) -> Result<FixtureVariables<'dae>, dae::DaeConstructionError> {
        let (real, integer) = dae.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Integer),
                    at,
                )?,
            ))
        })?;
        dae.variables(|variables| {
            Ok(FixtureVariables {
                a: variables.discrete_real(
                    VarName::new("a"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                b: variables.discrete_real(
                    VarName::new("b"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                n: variables.discrete_value(
                    VarName::new("n"),
                    integer,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                a_alias: variables.algebraic(
                    VarName::new("aAlias"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                n_alias: variables.discrete_value(
                    VarName::new("nAlias"),
                    integer,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                ambiguous: variables.algebraic(
                    VarName::new("ambiguous"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            })
        })
    }

    fn define_fixture_rows<'dae>(
        dae: &mut dae::DaeConstruction<'dae>,
        variables: FixtureVariables<'dae>,
        at: dae::DaeProvenance,
    ) -> Result<FixtureRows<'dae>, dae::DaeConstructionError> {
        dae.expressions(|expressions| {
            let a_read = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteReal(variables.a))?;
            let b_read = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteReal(variables.b))?;
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::PreDiscreteReal(variables.a))?;
            let n_read = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteValue(variables.n))?;
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteValue(variables.n_alias))?;
            let a_alias_read = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variables.a_alias))?;
            let ambiguous_read = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variables.ambiguous))?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            Ok(FixtureRows {
                alias: expressions.at(at).binary(
                    dae::BinaryOperator::Subtract,
                    a_alias_read,
                    a_read,
                )?,
                ambiguous: [
                    expressions.at(at).binary(
                        dae::BinaryOperator::Subtract,
                        ambiguous_read,
                        a_read,
                    )?,
                    expressions.at(at).binary(
                        dae::BinaryOperator::Subtract,
                        ambiguous_read,
                        b_read,
                    )?,
                ],
                n_value: one,
                n_alias_value: n_read,
            })
        })
    }

    fn define_fixture_equations<'dae>(
        dae: &mut dae::DaeConstruction<'dae>,
        wiring: Wiring,
        variables: FixtureVariables<'dae>,
        rows: FixtureRows<'dae>,
        at: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        let ambiguous = if wiring == Wiring::AliasesAndAmbiguousWriter {
            rows.ambiguous.as_slice()
        } else {
            &[]
        };
        dae.continuous(|continuous| {
            continuous.equation(at, |equation| equation.residual(rows.alias))?;
            for &residual in ambiguous {
                continuous.equation(at, |equation| equation.residual(residual))?;
            }
            Ok(())
        })?;
        dae.b1c([variables.n, variables.n_alias], |topology| {
            topology.owner(at, [variables.n], |owner| {
                owner.always(at, [(rows.n_value, at)])
            })?;
            topology.owner(at, [variables.n_alias], |owner| {
                owner.always(at, [(rows.n_alias_value, at)])
            })?;
            Ok(())
        })
    }

    fn fixture(wiring: Wiring) -> dae::Dae {
        let mut sources = SourceMap::new();
        let text = "discrete Real a; discrete Real b; discrete Integer n; \
                    Real aAlias; Integer nAlias; Real ambiguous;";
        let source = sources.add("same-tick.mo", text);
        let span = Span::from_offsets(source, 0, text.len());
        let at = dae::DaeProvenance::source(span).unwrap();
        dae::Dae::construct(sources, |dae| {
            let variables = define_fixture_variables(dae, at)?;
            let rows = define_fixture_rows(dae, variables, at)?;
            define_fixture_equations(dae, wiring, variables, rows, at)
        })
        .expect("the same-tick fixture is a valid DAE")
    }

    /// One branded view plus the derived definitions, so a test names
    /// coordinates the way the model author wrote them.
    struct Probe<'dae> {
        view: dae::DaeView<'dae>,
        definitions: SameTickDefinitions<'dae>,
    }

    fn probe<R>(wiring: Wiring, body: impl for<'dae> FnOnce(&Probe<'dae>) -> R) -> R {
        fixture(wiring).inspect(|view| {
            let causal = CausalDefinitions::derive(view);
            let definitions = SameTickDefinitions::derive(view, &causal);
            body(&Probe { view, definitions })
        })
    }

    impl<'dae> Probe<'dae> {
        fn variable(&self, name: &str) -> dae::VariableId<'dae> {
            self.view
                .variables()
                .find(|(_, node)| node.name().as_str() == name)
                .map(|(id, _)| id)
                .unwrap_or_else(|| panic!("fixture declares `{name}`"))
        }

        fn index(&self, name: &str) -> u32 {
            self.variable(name).index()
        }

        /// The bare same-instant read of one coordinate.
        fn read_of(&self, name: &str) -> dae::ExprId<'dae> {
            let target = self.index(name);
            (0..self.view.expression_count())
                .filter_map(|index| self.view.expression_id(index))
                .find(|&expression| {
                    let mut reads = BTreeSet::new();
                    collect_same_instant_reads(self.view, expression, &mut reads);
                    reads.len() == 1
                        && reads.contains(&target)
                        && matches!(
                            self.view
                                .expression(expression)
                                .map(|node| node.operation()),
                            Some(dae::ExpressionOperation::Coordinate(_))
                        )
                })
                .unwrap_or_else(|| panic!("fixture contains a bare read of `{name}`"))
        }

        /// The one declaration span the fixture was built at.
        fn span(&self) -> Span {
            let expression = self
                .view
                .expression_id(0)
                .expect("the fixture declares expressions");
            self.view
                .expression(expression)
                .expect("checked fixture expression resolves")
                .provenance()
                .span()
        }

        fn producer(&self, target: &str, reads: &[dae::ExprId<'dae>]) -> SameTickProducer<'dae> {
            self.producer_on(target, reads, &[0])
        }

        fn producer_on(
            &self,
            target: &str,
            reads: &[dae::ExprId<'dae>],
            clock_domains: &[u32],
        ) -> SameTickProducer<'dae> {
            SameTickProducer {
                clock_domains: clock_domains.to_vec(),
                targets: vec![self.variable(target)],
                value_reads: reads.to_vec(),
                condition_reads: Vec::new(),
                span: self.span(),
            }
        }

        fn issue(
            &self,
            producers: &[SameTickProducer<'dae>],
        ) -> Result<Vec<SameTickStep<'dae>>, SameTickOrderError> {
            issue_same_tick_schedule(self.view, &self.definitions, producers, &BTreeSet::new())
                .map(|schedule| schedule.steps)
        }

        /// The issued schedule rendered as names, so an assertion states the
        /// order a reader can check against the model text.
        fn issue_names(&self, producers: &[SameTickProducer<'dae>]) -> Vec<String> {
            self.issue(producers)
                .expect("the fixture schedule is issuable")
                .into_iter()
                .map(|step| match step {
                    SameTickStep::Producer(index) => {
                        variable_name(self.view, producers[index].targets[0].index())
                    }
                    SameTickStep::IntermediateDefinition { variable, .. } => {
                        format!("<{}>", variable_name(self.view, variable.index()))
                    }
                })
                .collect()
        }
    }

    #[test]
    fn a_reverse_ordered_reader_is_issued_after_its_producer() {
        probe(Wiring::Aliases, |probe| {
            let a_read = probe.read_of("a");
            // Caller order is reader-first, exactly as the model text writes it.
            let producers = [probe.producer("b", &[a_read]), probe.producer("a", &[])];
            assert_eq!(probe.issue_names(&producers), ["a", "b"]);
        });
    }

    #[test]
    fn an_exact_alias_is_issued_between_its_producer_and_its_reader() {
        probe(Wiring::Aliases, |probe| {
            let alias_read = probe.read_of("aAlias");
            let producers = [probe.producer("b", &[alias_read]), probe.producer("a", &[])];
            assert_eq!(probe.issue_names(&producers), ["a", "<aAlias>", "b"]);
        });
    }

    /// The F2 regression: a discrete-VALUE producer read through an alias the
    /// causal elimination declines. Before the fix `nAlias` was silently
    /// treated as current at tick entry, so the reader consumed the previous
    /// tick's `n` and the issued schedule contained no intermediate at all.
    #[test]
    fn an_alias_of_a_discrete_value_producer_is_issued_as_an_intermediate() {
        probe(Wiring::Aliases, |probe| {
            assert!(
                CausalDefinitions::derive(probe.view)
                    .definition_for_variable(probe.variable("nAlias"))
                    .is_none(),
                "the causal elimination must not own this alias, or the test \
                 would not exercise the same-tick derivation"
            );
            let alias_read = probe.read_of("nAlias");
            let producers = [probe.producer("b", &[alias_read]), probe.producer("n", &[])];
            assert_eq!(probe.issue_names(&producers), ["n", "<nAlias>", "b"]);
        });
    }

    /// The F3 regression: inserting the alias must not hide the cycle. Before
    /// the fix this exact shape was accepted and executed in an invented order.
    #[test]
    fn a_cycle_through_a_discrete_value_alias_is_rejected() {
        probe(Wiring::Aliases, |probe| {
            let alias_read = probe.read_of("nAlias");
            let b_read = probe.read_of("b");
            let producers = [
                probe.producer("b", &[alias_read]),
                probe.producer("n", &[b_read]),
            ];
            let error = probe
                .issue(&producers)
                .expect_err("a same-tick cycle through an alias must be rejected");
            assert!(
                matches!(error, SameTickOrderError::Cycle { .. }),
                "expected a cycle rejection, got {error:?}"
            );
        });
    }

    #[test]
    fn a_direct_same_tick_cycle_is_rejected() {
        probe(Wiring::Aliases, |probe| {
            let a_read = probe.read_of("a");
            let b_read = probe.read_of("b");
            let producers = [
                probe.producer("b", &[a_read]),
                probe.producer("a", &[b_read]),
            ];
            assert!(matches!(
                probe.issue(&producers),
                Err(SameTickOrderError::Cycle { .. })
            ));
        });
    }

    #[test]
    fn opposite_reads_on_disjoint_clocks_observe_held_entry_values() {
        probe(Wiring::Aliases, |probe| {
            let a_read = probe.read_of("a");
            let b_read = probe.read_of("b");
            let producers = [
                probe.producer_on("b", &[a_read], &[1]),
                probe.producer_on("a", &[b_read], &[2]),
            ];
            assert_eq!(probe.issue_names(&producers), ["b", "a"]);
        });
    }

    #[test]
    fn one_shared_domain_keeps_a_multi_clock_cycle_rejected() {
        probe(Wiring::Aliases, |probe| {
            let a_read = probe.read_of("a");
            let b_read = probe.read_of("b");
            let producers = [
                probe.producer_on("b", &[a_read], &[1, 2]),
                probe.producer_on("a", &[b_read], &[2, 3]),
            ];
            assert!(matches!(
                probe.issue(&producers),
                Err(SameTickOrderError::Cycle { .. })
            ));
        });
    }

    /// SDO-002's exception set: a history-lane read carries a strictly earlier
    /// instant, so it orders nothing. Two producers reading each other's `pre`
    /// lane are simultaneous, not cyclic.
    #[test]
    fn history_lane_reads_order_nothing() {
        probe(Wiring::Aliases, |probe| {
            let pre_a = (0..probe.view.expression_count())
                .filter_map(|index| probe.view.expression_id(index))
                .find(|&expression| {
                    matches!(
                        probe
                            .view
                            .expression(expression)
                            .map(|node| node.operation()),
                        Some(dae::ExpressionOperation::Coordinate(
                            dae::CoordinateView::PreDiscreteReal(_)
                        ))
                    )
                })
                .expect("fixture exposes a pre() coordinate");
            let mut reads = BTreeSet::new();
            collect_same_instant_reads(probe.view, pre_a, &mut reads);
            assert!(reads.is_empty(), "a pre() lane is not a same-instant read");
            let a_read = probe.read_of("a");
            let producers = [probe.producer("b", &[pre_a]), probe.producer("a", &[pre_a])];
            assert_eq!(probe.issue_names(&producers), ["b", "a"]);
            // ... while the ordinary read of the same coordinate does order.
            let ordered = [probe.producer("b", &[a_read]), probe.producer("a", &[])];
            assert_eq!(probe.issue_names(&ordered), ["a", "b"]);
        });
    }

    #[test]
    fn two_producers_claiming_one_target_are_rejected() {
        probe(Wiring::Aliases, |probe| {
            let producers = [probe.producer("a", &[]), probe.producer("a", &[])];
            assert!(matches!(
                probe.issue(&producers),
                Err(SameTickOrderError::DuplicateOwner { .. })
            ));
        });
    }

    /// A target an event transaction owns commits before the issued order runs,
    /// so it is current at tick entry. An always-active discrete definition
    /// that aliases it still refreshes before a clocked consumer; otherwise
    /// the consumer would read the alias's previous-event storage.
    #[test]
    fn a_transaction_owned_read_is_current_at_tick_entry() {
        probe(Wiring::Aliases, |probe| {
            let alias_read = probe.read_of("nAlias");
            let producers = [probe.producer("b", &[alias_read])];
            let excluded = BTreeSet::from([probe.index("n")]);
            let steps =
                issue_same_tick_schedule(probe.view, &probe.definitions, &producers, &excluded)
                    .expect("a transaction-owned read is schedulable")
                    .steps;
            assert!(matches!(
                steps.as_slice(),
                [
                    SameTickStep::IntermediateDefinition { variable, .. },
                    SameTickStep::Producer(0)
                ] if variable.index() == probe.index("nAlias")
            ));
        });
    }

    #[test]
    fn an_unclocked_discrete_definition_refreshes_before_its_clocked_reader() {
        probe(Wiring::Aliases, |probe| {
            let alias_read = probe.read_of("nAlias");
            let producers = [probe.producer("b", &[alias_read])];
            assert_eq!(probe.issue_names(&producers), ["<n>", "<nAlias>", "b"]);
        });
    }

    /// Failing closed: a coordinate this module cannot refresh, whose value
    /// depends on a producer of the partition, is a construction rejection
    /// rather than a silent tick-entry read.
    #[test]
    fn a_producer_reachable_read_with_no_exact_definition_fails_construction() {
        probe(Wiring::AliasesAndAmbiguousWriter, |probe| {
            assert!(
                probe
                    .definitions
                    .definition(probe.index("ambiguous"))
                    .is_none(),
                "two writers leave no exact definition"
            );
            let ambiguous_read = probe.read_of("ambiguous");
            let producers = [
                probe.producer("b", &[ambiguous_read]),
                probe.producer("a", &[]),
            ];
            let error = probe
                .issue(&producers)
                .expect_err("an unrefreshable producer-reachable read must fail");
            assert!(
                matches!(
                    &error,
                    SameTickOrderError::UnrefreshableRead { name, .. } if name == "ambiguous"
                ),
                "expected an unrefreshable-read rejection naming `ambiguous`, got {error:?}"
            );
        });
    }

    /// The same unrefreshable coordinate is harmless when nothing it depends on
    /// is produced by the partition: its tick-entry value holds for the tick.
    #[test]
    fn an_unrefreshable_read_that_reaches_no_producer_is_admitted() {
        probe(Wiring::AliasesAndAmbiguousWriter, |probe| {
            let ambiguous_read = probe.read_of("ambiguous");
            let producers = [probe.producer("n", &[ambiguous_read])];
            assert_eq!(probe.issue_names(&producers), ["n"]);
        });
    }

    /// The fusion oracle: program granularity must see through the same aliases
    /// the issued order does, or restoring family fusion would hide a same-tick
    /// read behind one fused entry read.
    #[test]
    fn the_read_closure_follows_aliases_to_their_producers() {
        probe(Wiring::Aliases, |probe| {
            let closure =
                probe
                    .definitions
                    .read_closure(probe.view, &[probe.read_of("nAlias")], &[]);
            assert!(closure.contains(&probe.index("nAlias")));
            assert!(
                closure.contains(&probe.index("n")),
                "the closure of a read of `nAlias` must reach `n`"
            );
            let unrelated = probe
                .definitions
                .read_closure(probe.view, &[probe.read_of("b")], &[]);
            assert_eq!(unrelated, BTreeSet::from([probe.index("b")]));
        });
    }
}
