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

mod event_reads;
mod intermediates;
pub use event_reads::{EventReadCollector, EventReadFacts, SameTickReadContext};
use intermediates::{IntermediateKey, IntermediateNode, discover_intermediates};

/// One clocked producer described by the Solve construction.
///
/// `value_reads` and `condition_reads` are the expression and condition roots
/// the producer evaluates at its tick. A producer whose reads all resolve
/// through a history lane (for example a `sample(u)` source) passes no roots.
pub struct SameTickProducer<'dae> {
    /// Construction-issued current/entry facts, or the synchronous boundary.
    pub read_context: SameTickReadContext<'dae>,
    /// Canonical periodic-clock domains on which this producer can execute.
    /// Same-tick exchange exists only between producers whose domains can
    /// fire on one shared instant under the caller's [`ClockCoincidence`]
    /// relation (identical domains always do); a never-coincident read
    /// observes held entry storage.
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

/// Which distinct periodic-clock domains can fire on one shared instant.
///
/// Same-tick value exchange between two producers exists exactly when their
/// clocks can tick together: on such an instant the reader must observe the
/// freshly issued value, while clocks that never coincide keep the held
/// entry-storage read (the SOLVE-C22 left-limit lane). Clock identity is not
/// the right test: two distinct periodic clocks whose phase difference is an
/// integer multiple of the gcd of their periods (a whole-interval
/// `shiftSample`, any `subSample`/`superSample`) fire together, so the caller
/// proves coincidence over its clock lattice and states the result here. The
/// relation is reflexive by construction and stored symmetric; it is a
/// required argument of [`issue_same_tick_schedule`] so no caller can skip
/// the coincidence question.
pub struct ClockCoincidence {
    /// Cross-identity pairs proved coincident, stored as `(low, high)`.
    coincident_pairs: BTreeSet<(u32, u32)>,
}

impl ClockCoincidence {
    /// The relation in which no two distinct clock domains ever share a tick.
    #[must_use]
    pub const fn never() -> Self {
        Self {
            coincident_pairs: BTreeSet::new(),
        }
    }

    /// The relation containing exactly `pairs` as coincident distinct-domain
    /// pairs, order-insensitively; a reflexive `(a, a)` entry is redundant and
    /// ignored because identical domains always coincide.
    #[must_use]
    pub fn of_pairs(pairs: impl IntoIterator<Item = (u32, u32)>) -> Self {
        Self {
            coincident_pairs: pairs
                .into_iter()
                .filter(|&(left, right)| left != right)
                .map(|(left, right)| (left.min(right), left.max(right)))
                .collect(),
        }
    }

    /// Whether domains `left` and `right` can fire on one shared instant.
    #[must_use]
    pub fn coincident(&self, left: u32, right: u32) -> bool {
        left == right
            || self
                .coincident_pairs
                .contains(&(left.min(right), left.max(right)))
    }

    /// Whether any domain of `lhs` can fire together with any domain of `rhs`.
    fn domains_overlap(&self, lhs: &[u32], rhs: &[u32]) -> bool {
        lhs.iter()
            .any(|&left| rhs.iter().any(|&right| self.coincident(left, right)))
    }
}

/// Identity issued with an intermediate's consumer-specific execution context.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SameTickIntermediateId(usize);

/// One issued step of the same-tick schedule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SameTickStep<'dae> {
    /// Execute the producer at the given caller index.
    Producer(usize),
    /// Refresh the exact definition of one algebraic intermediate into private
    /// work state. The definition travels with the step: the consumer lowers
    /// exactly the expression this module ordered, never one it re-derives.
    IntermediateDefinition {
        identity: SameTickIntermediateId,
        variable: dae::VariableId<'dae>,
        definition: dae::ExprId<'dae>,
        /// This row refreshes an ordinary event value, not an MLS16 history view.
        read_boundary: SameTickReadBoundary,
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
    pub intermediate_consumers: BTreeMap<SameTickIntermediateId, Vec<usize>>,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum SameTickOrderError {
    #[error(transparent)]
    Projection(#[from] rumoca_eval_dae::ProjectionError),
    #[error("derivative dependency has no structurally matched scalar source row")]
    MissingDerivative { span: Span },
    #[error("same-tick derivative dependency requires a non-record scalar view")]
    UnsupportedDerivativeValue { span: Span },
    #[error("same-tick producer clock domains are not a nonempty canonical set")]
    InvalidClockDomains { span: Span },
    #[error(
        "same-tick discrete producers form an algebraic loop with no pre()/previous() boundary: {detail}"
    )]
    Cycle { span: Span, detail: String },
    #[error("variable `{name}` is written by more than one same-tick producer")]
    DuplicateOwner { span: Span, name: String },
    #[error(
        "same-tick clock exchange for selected discrete-value array `{name}` requires element-aware producer ownership"
    )]
    UnsupportedSelectedTarget { span: Span, name: String },
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
            Self::Projection(error) => error.span(),
            Self::MissingDerivative { span }
            | Self::UnsupportedDerivativeValue { span }
            | Self::InvalidClockDomains { span }
            | Self::Cycle { span, .. }
            | Self::DuplicateOwner { span, .. }
            | Self::UnsupportedSelectedTarget { span, .. }
            | Self::UnrefreshableRead { span, .. } => *span,
        }
    }
}

/// One ephemeral scalar view of the actual structurally matched derivative row.
#[derive(Clone)]
pub struct MatchedDerivativeSource<'dae> {
    pub state: dae::StateId<'dae>,
    pub state_scalar: usize,
    pub expression: dae::ExprId<'dae>,
    pub expression_scalar: usize,
    pub domain_point: Option<(dae::DomainId<'dae>, Vec<i64>)>,
}

type DerivativeSources<'dae> = BTreeMap<(u32, usize), MatchedDerivativeSource<'dae>>;

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
    derivative_sources: DerivativeSources<'dae>,
    ids: Vec<Option<dae::VariableId<'dae>>>,
    exact: BTreeMap<u32, dae::ExprId<'dae>>,
    opaque: BTreeMap<u32, OwnerReadViews>,
    /// Exact always-active discrete definitions must execute inside a clock
    /// partition when one of its producers reads them. Their stored entry
    /// value predates the tick, even when they do not depend on another
    /// clock-owned producer (for example `fresh = valid and sample(...)`).
    refresh_on_tick: BTreeSet<u32>,
    selected_variables: BTreeSet<u32>,
}

/// One coordinate's accumulated writers while [`SameTickDefinitions`] derives.
#[derive(Default)]
struct WriterAccumulator<'dae> {
    count: usize,
    definition: Option<dae::ExprId<'dae>>,
    reads: OwnerReadViews,
}

/// Executable read facts for the two existing semantic boundaries.
#[derive(Default)]
struct OwnerReadViews {
    current: BTreeSet<u32>,
    clock_partition: BTreeSet<u32>,
}

/// Executable dependency boundary of an issued intermediate refresh.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SameTickReadBoundary {
    Current,
    ClockPartition,
}

type ReadRole = SameTickReadBoundary;

mod reads;
pub use reads::{collect_condition_same_instant_reads, collect_same_instant_reads};

type Writers<'dae> = BTreeMap<u32, WriterAccumulator<'dae>>;

impl<'dae> SameTickDefinitions<'dae> {
    /// Derive the exact-definition map for one branded DAE view.
    ///
    /// The plan's [`CausalDefinitions`] are authoritative wherever they apply:
    /// they already prove orientation and acyclicity for whole algebraic
    /// aliases, and the plan orients every two-sided discrete Real row. Holding
    /// a derived plan is the precondition, so a row the plan cannot orient is
    /// reported by its derivation instead of silently left undefined here. This
    /// derivation adds the exact single-writer definitions that proof declines
    /// (a widening `Real x = integerDiscrete`, a `B.1c` value owner, an exact
    /// generated connection row) and records every other writer as opaque.
    pub fn derive(
        view: dae::DaeView<'dae>,
        plan: &crate::CausalDiscretePlan<'dae>,
    ) -> Result<Self, SameTickOrderError> {
        Self::derive_with_derivative_sources(view, plan, std::iter::empty())
    }

    /// Extend the exact dependency plan with source rows from structural matching.
    /// Solve supplies the same matched rows it substitutes for derivative reads;
    /// no matching or equation orientation is performed by this query.
    pub fn derive_with_derivative_sources(
        view: dae::DaeView<'dae>,
        plan: &crate::CausalDiscretePlan<'dae>,
        sources: impl IntoIterator<Item = MatchedDerivativeSource<'dae>>,
    ) -> Result<Self, SameTickOrderError> {
        let derivative_sources = sources
            .into_iter()
            .map(|source| ((source.state.index(), source.state_scalar), source))
            .collect::<DerivativeSources<'dae>>();
        let causal: &CausalDefinitions<'dae> = plan.causal_definitions();
        let mut ids = vec![None; view.variable_count()];
        for (id, _) in view.variables() {
            if let Some(slot) = ids.get_mut(id.index() as usize) {
                *slot = Some(id);
            }
        }
        let mut writers = Writers::new();
        let mut refresh_on_tick = BTreeSet::new();
        collect_continuous_writers(view, &derivative_sources, &mut writers)?;
        collect_discrete_real_writers(
            view,
            plan,
            &derivative_sources,
            &mut writers,
            &mut refresh_on_tick,
        )?;
        collect_discrete_value_writers(
            view,
            &derivative_sources,
            &mut writers,
            &mut refresh_on_tick,
        )?;

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
        let selected_variables = (0..view.discrete_value_owner_count())
            .filter_map(|index| view.discrete_value_owner_id(index))
            .filter_map(|id| view.discrete_value_owner(id))
            .flat_map(|owner| owner.targets().iter())
            .filter(|target| target.scalar_selection().is_some())
            .map(|target| target.variable().index())
            .collect();
        Ok(Self {
            derivative_sources,
            ids,
            exact,
            opaque,
            refresh_on_tick,
            selected_variables,
        })
    }

    /// The exact definition a same-tick consumer refreshes this coordinate from.
    #[must_use]
    pub fn definition(&self, variable: u32) -> Option<dae::ExprId<'dae>> {
        self.exact.get(&variable).copied()
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
}

fn record_definition<'dae>(
    view: dae::DaeView<'dae>,
    sources: &DerivativeSources<'dae>,
    writers: &mut Writers<'dae>,
    target: u32,
    value: dae::ExprId<'dae>,
) -> Result<(), SameTickOrderError> {
    let entry = writers.entry(target).or_default();
    entry.count += 1;
    entry.definition = (entry.count == 1).then_some(value);
    collect_writer_reads(view, value, sources, &mut entry.reads)?;
    Ok(())
}

fn collect_writer_reads<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::ExprId<'dae>,
    sources: &DerivativeSources<'dae>,
    reads: &mut OwnerReadViews,
) -> Result<(), SameTickOrderError> {
    reads::collect_expression_reads(view, value, sources, &|_| false, &mut reads.current)?;
    collect_same_instant_reads(view, value, &mut reads.clock_partition);
    Ok(())
}

fn record_opaque_writer<'dae>(writers: &mut Writers<'dae>, target: u32, reads: &OwnerReadViews) {
    let entry = writers.entry(target).or_default();
    entry.count += 1;
    entry.definition = None;
    entry.reads.current.extend(reads.current.iter().copied());
    entry
        .reads
        .clock_partition
        .extend(reads.clock_partition.iter().copied());
}

/// Continuous residual owners contribute a definition only in the exact
/// `algebraic - value` shape. A whole-alias pair (`x - y = 0`) is left to
/// [`CausalDefinitions`], which owns its orientation proof.
fn collect_continuous_writers<'dae>(
    view: dae::DaeView<'dae>,
    sources: &DerivativeSources<'dae>,
    writers: &mut Writers<'dae>,
) -> Result<(), SameTickOrderError> {
    let derivative_rows = reads::complete_derivative_residuals(view, sources);
    for owner in view.continuous_owners() {
        let residual = match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => equation.residual(),
            dae::ContinuousOwnerView::Structured { family, .. } => {
                let Some(body) =
                    crate::causal_definitions::exact_row_major_family_body(view, family)
                else {
                    continue;
                };
                body
            }
        };
        if derivative_rows.contains(&residual) {
            continue;
        }
        if let Some((target, value)) =
            single_coordinate_definition(view, residual, CoordinateKinds::Algebraic)
        {
            record_definition(view, sources, writers, target, value)?;
        }
    }
    Ok(())
}

/// Always-active discrete Real equations (the generated connection rows) may
/// define a whole algebraic or discrete-Real coordinate.
fn collect_discrete_real_writers<'dae>(
    view: dae::DaeView<'dae>,
    plan: &crate::CausalDiscretePlan<'dae>,
    sources: &DerivativeSources<'dae>,
    writers: &mut Writers<'dae>,
    refresh_on_tick: &mut BTreeSet<u32>,
) -> Result<(), SameTickOrderError> {
    for index in 0..view.discrete_real_equation_count() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        if equation.activation() != dae::DiscreteRealActivation::Always {
            continue;
        }
        // A row between two discrete Real coordinates (a connection) names no
        // side by shape; its proven orientation is the causal discrete plan's.
        let definition = single_coordinate_definition(
            view,
            equation.residual(),
            CoordinateKinds::AlgebraicOrDiscreteReal,
        );
        let definition = if definition.is_some() {
            definition
        } else if let Some(oriented) = plan.discrete_real_definition(index) {
            let target = dae::VariableId::from(oriented.target()).index();
            // One element of an array coordinate is not an exact definition of
            // the whole coordinate: it is one of several writers.
            if oriented.element().is_some() {
                let mut reads = OwnerReadViews::default();
                collect_writer_reads(view, oriented.value(), sources, &mut reads)?;
                record_opaque_writer(writers, target, &reads);
                None
            } else {
                Some((target, oriented.value()))
            }
        } else {
            None
        };
        if let Some((target, value)) = definition {
            record_definition(view, sources, writers, target, value)?;
            refresh_on_tick.insert(target);
        }
    }
    Ok(())
}

/// `B.1c` value owners name their targets explicitly, so both their exact
/// unconditional definitions and their unrefreshable conditional forms are
/// recorded without guessing an orientation.
fn collect_discrete_value_writers<'dae>(
    view: dae::DaeView<'dae>,
    sources: &DerivativeSources<'dae>,
    writers: &mut Writers<'dae>,
    refresh_on_tick: &mut BTreeSet<u32>,
) -> Result<(), SameTickOrderError> {
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
                record_discrete_value_writer(
                    view,
                    sources,
                    writers,
                    refresh_on_tick,
                    target,
                    value,
                )?;
            }
            continue;
        }
        let mut reads = OwnerReadViews::default();
        for branch in branches.iter() {
            for (value, _) in branch.values().iter() {
                collect_writer_reads(view, value, sources, &mut reads)?;
            }
        }
        for target in owner.targets().iter() {
            record_opaque_writer(writers, dae::VariableId::from(target).index(), &reads);
        }
    }
    Ok(())
}

fn record_discrete_value_writer<'dae>(
    view: dae::DaeView<'dae>,
    sources: &DerivativeSources<'dae>,
    writers: &mut Writers<'dae>,
    refresh_on_tick: &mut BTreeSet<u32>,
    target: dae::DiscreteValueTarget<'dae>,
    value: dae::ExprId<'dae>,
) -> Result<(), SameTickOrderError> {
    let variable = target.variable().index();
    if target.scalar_selection().is_some() {
        let mut reads = OwnerReadViews::default();
        collect_writer_reads(view, value, sources, &mut reads)?;
        record_opaque_writer(writers, variable, &reads);
    } else {
        record_definition(view, sources, writers, variable, value)?;
        refresh_on_tick.insert(variable);
    }
    Ok(())
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
    coincidence: &ClockCoincidence,
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
    for producer in producers {
        if !matches!(producer.read_context, SameTickReadContext::ClockPartition) {
            continue;
        }
        if let Some(target) = producer
            .targets
            .iter()
            .find(|target| definitions.selected_variables.contains(&target.index()))
        {
            return Err(SameTickOrderError::UnsupportedSelectedTarget {
                span: producer.span,
                name: variable_name(view, target.index()),
            });
        }
    }
    let producer_of = map_producer_targets(view, producers)?;
    let direct_reads = collect_producer_reads(view, producers)?;
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
        coincidence,
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
) -> Result<Vec<BTreeSet<u32>>, SameTickOrderError> {
    producers
        .iter()
        .map(|producer| {
            if let SameTickReadContext::OrdinaryEvent(facts) = &producer.read_context {
                return Ok(facts.current().clone());
            }
            let mut reads = BTreeSet::new();
            for &value in &producer.value_reads {
                collect_same_instant_reads(view, value, &mut reads);
            }
            for &condition in &producer.condition_reads {
                collect_condition_same_instant_reads(view, condition, &mut reads);
            }
            Ok(reads)
        })
        .collect()
}

/// Node universe: producers in caller order, then intermediates by variable
/// index. Emission scans this fixed sequence, so the issued order is
/// deterministic and biased toward source order, exactly like the proven GALEC
/// `order_clocked_assignments`.
#[derive(Clone, Copy)]
enum Node {
    Producer(usize),
    Intermediate(IntermediateKey),
}

/// Emit the topological order, or reject the same-tick cycle that blocks it.
fn emit_schedule<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &SameTickDefinitions<'dae>,
    producers: &[SameTickProducer<'dae>],
    producer_of: &BTreeMap<u32, usize>,
    direct_reads: &[BTreeSet<u32>],
    intermediates: &BTreeMap<IntermediateKey, IntermediateNode<'dae>>,
    coincidence: &ClockCoincidence,
) -> Result<SameTickSchedule<'dae>, SameTickOrderError> {
    let event_order = event_reads::EventOrdering::derive(producers, producer_of, coincidence);
    let graph = ScheduleGraph {
        event_order,
        producers,
        direct_reads,
        intermediates,
        coincidence,
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
                    && graph.entry_readers_emitted(node, &emitted)
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
                identity: SameTickIntermediateId(graph.node_of_intermediate[&variable]),
                variable: definitions
                    .variable_id(variable.variable)
                    .expect("a discovered intermediate has a DAE variable identity"),
                definition: intermediates[&variable]
                    .definition
                    .expect("a kept intermediate has an exact definition"),
                read_boundary: variable.role,
            },
        });
    }
    if steps.len() != graph.nodes.len() {
        return Err(graph.cycle_error(view, &emitted));
    }
    let intermediate_consumers = intermediates
        .iter()
        .map(|(&variable, node)| {
            (
                SameTickIntermediateId(graph.node_of_intermediate[&variable]),
                node.consumers.iter().copied().collect(),
            )
        })
        .collect();
    Ok(SameTickSchedule {
        steps,
        intermediate_consumers,
    })
}

/// The read adjacency the emission loop and its rejection diagnostic share.
struct ScheduleGraph<'a, 'dae> {
    event_order: event_reads::EventOrdering,
    producers: &'a [SameTickProducer<'dae>],
    direct_reads: &'a [BTreeSet<u32>],
    intermediates: &'a BTreeMap<IntermediateKey, IntermediateNode<'dae>>,
    coincidence: &'a ClockCoincidence,
    node_of_intermediate: BTreeMap<IntermediateKey, usize>,
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
            let reader = match consumer {
                Node::Producer(reader) => Some(reader),
                Node::Intermediate(key) => key.current_consumer,
            };
            if reader.is_some_and(|reader| !self.event_order.requires_current(reader, producer)) {
                return None;
            }
            return self
                .node_overlaps_producer(consumer, producer)
                .then_some(producer);
        }
        let role = match consumer {
            Node::Intermediate(key) => key.role,
            Node::Producer(reader) => match self.producers[reader].read_context {
                SameTickReadContext::ClockPartition => ReadRole::ClockPartition,
                SameTickReadContext::OrdinaryEvent(_) => ReadRole::Current,
            },
        };
        let current_consumer = match consumer {
            Node::Producer(reader) => (role == ReadRole::Current).then_some(reader),
            Node::Intermediate(key) => key.current_consumer,
        };
        let key = IntermediateKey {
            variable,
            role,
            current_consumer,
        };
        if let Node::Producer(reader) = consumer
            && !self.intermediates.get(&key)?.consumers.contains(&reader)
        {
            return None;
        }
        self.node_of_intermediate.get(&key).copied()
    }

    fn entry_readers_emitted(&self, node: Node, emitted: &[bool]) -> bool {
        let Node::Producer(producer) = node else {
            return true;
        };
        self.event_order
            .entry_readers(producer)
            .iter()
            .all(|&reader| emitted[reader])
    }

    fn node_overlaps_producer(&self, node: Node, producer: usize) -> bool {
        let owner_domains = &self.producers[producer].clock_domains;
        match node {
            Node::Producer(index) => self
                .coincidence
                .domains_overlap(&self.producers[index].clock_domains, owner_domains),
            Node::Intermediate(variable) => {
                self.intermediates[&variable]
                    .consumers
                    .iter()
                    .any(|&consumer| {
                        self.coincidence
                            .domains_overlap(&self.producers[consumer].clock_domains, owner_domains)
                    })
            }
        }
    }

    /// Report the same-tick cycle that left the schedule unfinished, preferring
    /// a blocked producer: it carries the source span the model author wrote.
    /// Among blocked producers, one with a named target is preferred, so an
    /// event transaction that projects no targets never leaves the message
    /// naming an empty string.
    fn cycle_error(&self, view: dae::DaeView<'_>, emitted: &[bool]) -> SameTickOrderError {
        let unemitted = || {
            self.nodes
                .iter()
                .copied()
                .enumerate()
                .filter(|(index, _)| !emitted[*index])
        };
        let (blocked_index, blocked_node) = unemitted()
            .find(|(_, node)| {
                matches!(node, Node::Producer(producer) if !self.producers[*producer].targets.is_empty())
            })
            .or_else(|| unemitted().find(|(_, node)| matches!(node, Node::Producer(_))))
            .or_else(|| unemitted().next())
            .expect("an unfinished schedule has an unemitted node");
        let (span, target_names) = match blocked_node {
            Node::Producer(producer) => (
                self.producers[producer].span,
                if self.producers[producer].targets.is_empty() {
                    "<unnamed clocked producer>".to_owned()
                } else {
                    self.producers[producer]
                        .targets
                        .iter()
                        .map(|target| variable_name(view, target.index()))
                        .collect::<Vec<_>>()
                        .join(", ")
                },
            ),
            Node::Intermediate(variable) => (
                self.producers
                    .iter()
                    .zip(self.direct_reads)
                    .find_map(|(producer, reads)| {
                        reads.contains(&variable.variable).then_some(producer.span)
                    })
                    .unwrap_or(self.producers[0].span),
                variable_name(view, variable.variable),
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

fn variable_name(view: dae::DaeView<'_>, variable: u32) -> String {
    view.variables()
        .find(|(id, _)| id.index() == variable)
        .map(|(_, node)| node.name().to_string())
        .unwrap_or_else(|| format!("#{variable}"))
}

#[cfg(test)]
mod tests {
    mod event_fact_tests;

    use rumoca_core::{SourceMap, Span, TypeId, VarName};

    use super::reads::{
        collect_condition_same_instant_reads_except, collect_condition_same_instant_reads_pruned,
    };
    use super::*;

    #[test]
    fn selected_value_writers_are_opaque_and_clock_exchange_is_typed_refused() {
        let text = "discrete Boolean m[2]; m[1]=true; m[2]=false;";
        let mut sources = SourceMap::new();
        let source = sources.add("SelectedClockOwner.mo", text);
        let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        let model = dae::Dae::construct(sources, |model| {
            let ty = model.types(|types| {
                types.derived(dae::ValueType::array(dae::ScalarType::Boolean, [2]), at)
            })?;
            let variable = model.variables(|variables| {
                variables.discrete_value(
                    VarName::new("m"),
                    ty,
                    at,
                    dae::VariableAttributes::default(),
                )
            })?;
            let targets = [
                model.discrete_value_element(variable, &[1], at)?,
                model.discrete_value_element(variable, &[2], at)?,
            ];
            let value = model.expressions(|expressions| {
                expressions.at(at).literal(dae::DaeLiteral::Boolean(true))
            })?;
            model.b1c_selected(targets, |topology| {
                topology.selected_owner(at, targets, |owner| owner.always(at, [(value, at); 2]))?;
                Ok(())
            })
        })
        .unwrap();
        model.inspect(|view| {
            let definitions = SameTickDefinitions::derive(view, &oriented_plan(view)).unwrap();
            let variable = view.variables().next().unwrap().0;
            assert!(definitions.definition(variable.index()).is_none());
            assert!(!definitions.refreshes_on_tick(variable.index()));
            let producer = SameTickProducer {
                read_context: SameTickReadContext::ClockPartition,
                clock_domains: vec![0],
                targets: vec![variable],
                value_reads: vec![],
                condition_reads: vec![],
                span: at.span(),
            };
            assert!(matches!(issue_same_tick_schedule(view, &definitions, &[producer], &BTreeSet::new(), &ClockCoincidence::never()),
                Err(SameTickOrderError::UnsupportedSelectedTarget { span, .. }) if span == at.span()));
        });
    }

    #[test]
    fn condition_read_projection_prunes_only_the_issued_expression() {
        let mut sources = SourceMap::new();
        let source = sources.add("entry-read.mo", "discrete Integer n; discrete Real a;");
        let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 35)).unwrap();
        let model = dae::Dae::construct(sources, |model| {
            let variables = define_fixture_variables(model, at)?;
            let rows = define_fixture_rows(model, variables, at)?;
            define_fixture_equations(model, Wiring::Aliases, variables, rows, at)?;
            let (integer_guard, real_guard) = model.expressions(|expressions| {
                let n = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::DiscreteValue(variables.n))?;
                let a = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::DiscreteReal(variables.a))?;
                let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
                let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                Ok((
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::GreaterEqual, n, one)?,
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Greater, a, zero)?,
                ))
            })?;
            model.conditions(|conditions| {
                let integer = conditions.reserve(at)?;
                conditions.define(integer, dae::ConditionInput::Discrete(integer_guard), at)?;
                let real = conditions.reserve(at)?;
                conditions.define(real, dae::ConditionInput::Discrete(real_guard), at)?;
                let both = conditions.reserve(at)?;
                conditions.define(both, dae::ConditionInput::And(integer, real), at)
            })
        })
        .unwrap();
        model.inspect(|view| {
            let root = view.conditions().last().unwrap().0;
            let dae::ConditionOperation::Discrete(integer_guard) =
                view.conditions().next().unwrap().1.operation()
            else {
                panic!("issued integer condition");
            };
            let index = |name: &str| {
                view.variables()
                    .find(|(_, v)| v.name().as_str() == name)
                    .unwrap()
                    .0
                    .index()
            };
            let mut direct = BTreeSet::new();
            collect_condition_same_instant_reads(view, root, &mut direct);
            assert_eq!(direct, BTreeSet::from([index("n"), index("a")]));
            let mut projected = BTreeSet::new();
            collect_condition_same_instant_reads_pruned(
                view,
                root,
                &BTreeSet::new(),
                |id| id == integer_guard,
                &mut projected,
            );
            assert_eq!(projected, BTreeSet::from([index("a")]));
        });
    }

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
        fixture_with(wiring, |_, _, _| Ok(()))
    }

    fn fixture_with(
        wiring: Wiring,
        extra: impl for<'dae> FnOnce(
            &mut dae::DaeConstruction<'dae>,
            FixtureVariables<'dae>,
            dae::DaeProvenance,
        ) -> Result<(), dae::DaeConstructionError>,
    ) -> dae::Dae {
        let mut sources = SourceMap::new();
        let text = "discrete Real a; discrete Real b; discrete Integer n; \
                    Real aAlias; Integer nAlias; Real ambiguous;";
        let source = sources.add("same-tick.mo", text);
        let span = Span::from_offsets(source, 0, text.len());
        let at = dae::DaeProvenance::source(span).unwrap();
        dae::Dae::construct(sources, |dae| {
            let variables = define_fixture_variables(dae, at)?;
            let rows = define_fixture_rows(dae, variables, at)?;
            define_fixture_equations(dae, wiring, variables, rows, at)?;
            extra(dae, variables, at)
        })
        .expect("the same-tick fixture is a valid DAE")
    }

    /// Distinct source activation and body predicates may read the same
    /// coordinate (MLS §8.5 / SOLVE-C58). Only the activation's issued
    /// condition identity is excluded, never all reads of that coordinate.
    fn trigger_body_fixture() -> dae::Dae {
        fixture_with(Wiring::Aliases, |dae, variables, at| {
            let values = dae.expressions(|expressions| {
                let current = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::DiscreteReal(variables.a))?;
                let previous = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::PreDiscreteReal(variables.a))?;
                let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                Ok([
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::GreaterEqual, current, zero)?,
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Greater, current, previous)?,
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Greater, previous, zero)?,
                ])
            })?;
            define_trigger_body_conditions(dae, values, at)
        })
    }

    fn define_trigger_body_conditions<'dae>(
        dae: &mut dae::DaeConstruction<'dae>,
        values: [dae::ExprId<'dae>; 3],
        at: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        dae.conditions(|conditions| {
            let mut ids = Vec::new();
            for value in values {
                let id = conditions.reserve(at)?;
                conditions.define(id, dae::ConditionInput::Discrete(value), at)?;
                ids.push(id);
            }
            for body in [ids[1], ids[2]] {
                let guard = conditions.reserve(at)?;
                conditions.define(guard, dae::ConditionInput::And(ids[0], body), at)?;
            }
            Ok(())
        })
    }

    #[test]
    fn trigger_identity_pruning_retains_the_same_coordinate_body_read() {
        trigger_body_fixture().inspect(|view| {
            let trigger = view.condition_id(0).unwrap();
            let body = view.condition_id(1).unwrap();
            let guard = view.condition_id(3).unwrap();
            assert_ne!(trigger, body, "independently issued predicates");
            let definitions = SameTickDefinitions::derive(view, &oriented_plan(view)).unwrap();
            let probe = Probe { view, definitions };
            let expected = BTreeSet::from([probe.index("a")]);
            let mut reads = BTreeSet::new();
            collect_condition_same_instant_reads(view, trigger, &mut reads);
            assert_eq!(reads, expected, "source trigger reads the coordinate");
            reads.clear();
            collect_condition_same_instant_reads_except(
                view,
                guard,
                &BTreeSet::from([trigger]),
                &mut reads,
            );
            assert_eq!(reads, expected, "body keeps its same-instant dependency");
        });
    }

    #[test]
    fn trigger_identity_pruning_omits_the_trigger_only_dependency() {
        trigger_body_fixture().inspect(|view| {
            let trigger = view.condition_id(0).unwrap();
            let mut reads = BTreeSet::new();
            collect_condition_same_instant_reads_except(
                view,
                trigger,
                &BTreeSet::from([trigger]),
                &mut reads,
            );
            assert!(
                reads.is_empty(),
                "entry-only activation orders no body read"
            );
        });
    }

    #[test]
    fn trigger_identity_pruning_keeps_pre_only_body_reads_in_the_history_lane() {
        trigger_body_fixture().inspect(|view| {
            let trigger = view.condition_id(0).unwrap();
            let guard = view.condition_id(4).unwrap();
            let mut reads = BTreeSet::new();
            collect_condition_same_instant_reads_except(
                view,
                guard,
                &BTreeSet::from([trigger]),
                &mut reads,
            );
            assert!(
                reads.is_empty(),
                "pre(a) does not observe a current producer"
            );
        });
    }

    /// One branded view plus the derived definitions, so a test names
    /// coordinates the way the model author wrote them.
    struct Probe<'dae> {
        view: dae::DaeView<'dae>,
        definitions: SameTickDefinitions<'dae>,
    }

    fn probe<R>(wiring: Wiring, body: impl for<'dae> FnOnce(&Probe<'dae>) -> R) -> R {
        fixture(wiring).inspect(|view| {
            let definitions = SameTickDefinitions::derive(view, &oriented_plan(view)).unwrap();
            body(&Probe { view, definitions })
        })
    }

    /// The fixture's causal discrete plan; every fixture row orients.
    fn oriented_plan(view: dae::DaeView<'_>) -> crate::CausalDiscretePlan<'_> {
        match crate::CausalDiscretePlan::derive(view) {
            Ok(plan) => plan,
            Err(error) => panic!("fixture discrete rows orient: {error}"),
        }
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
                read_context: SameTickReadContext::ClockPartition,
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
            self.issue_with(producers, &ClockCoincidence::never())
        }

        fn issue_with(
            &self,
            producers: &[SameTickProducer<'dae>],
            coincidence: &ClockCoincidence,
        ) -> Result<Vec<SameTickStep<'dae>>, SameTickOrderError> {
            issue_same_tick_schedule(
                self.view,
                &self.definitions,
                producers,
                &BTreeSet::new(),
                coincidence,
            )
            .map(|schedule| schedule.steps)
        }

        /// The issued schedule rendered as names, so an assertion states the
        /// order a reader can check against the model text.
        fn issue_names(&self, producers: &[SameTickProducer<'dae>]) -> Vec<String> {
            self.issue_names_with(producers, &ClockCoincidence::never())
        }

        fn issue_names_with(
            &self,
            producers: &[SameTickProducer<'dae>],
            coincidence: &ClockCoincidence,
        ) -> Vec<String> {
            self.issue_with(producers, coincidence)
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

    /// A whole-interval `shiftSample`/`subSample` reader lives on a clock
    /// identity distinct from its source's, yet the two clocks fire together.
    /// With the coincidence proved, the cross-clock read orders exactly like a
    /// same-clock read, independent of caller (source equation) order.
    #[test]
    fn coincident_domains_order_a_cross_clock_read() {
        probe(Wiring::Aliases, |probe| {
            let a_read = probe.read_of("a");
            let coincidence = ClockCoincidence::of_pairs([(1, 2)]);
            // Caller order is reader-first, exactly as the model text writes it.
            let producers = [
                probe.producer_on("b", &[a_read], &[2]),
                probe.producer_on("a", &[], &[1]),
            ];
            assert_eq!(probe.issue_names_with(&producers, &coincidence), ["a", "b"]);
            // The owner-first caller order issues the same schedule.
            let swapped = [
                probe.producer_on("a", &[], &[1]),
                probe.producer_on("b", &[a_read], &[2]),
            ];
            assert_eq!(probe.issue_names_with(&swapped, &coincidence), ["a", "b"]);
        });
    }

    /// The same producers with no coincidence proof keep the held-entry read:
    /// caller order is preserved because no exchange edge exists.
    #[test]
    fn never_coincident_domains_keep_the_caller_order() {
        probe(Wiring::Aliases, |probe| {
            let a_read = probe.read_of("a");
            let producers = [
                probe.producer_on("b", &[a_read], &[2]),
                probe.producer_on("a", &[], &[1]),
            ];
            assert_eq!(probe.issue_names(&producers), ["b", "a"]);
        });
    }

    /// Mutual same-instant reads across two coincident clocks are a genuine
    /// same-tick algebraic loop and are rejected, mirroring the cross-clock
    /// construction rejection in the Solve lowering.
    #[test]
    fn coincident_opposite_reads_are_rejected_as_a_cycle() {
        probe(Wiring::Aliases, |probe| {
            let a_read = probe.read_of("a");
            let b_read = probe.read_of("b");
            let producers = [
                probe.producer_on("b", &[a_read], &[1]),
                probe.producer_on("a", &[b_read], &[2]),
            ];
            assert!(matches!(
                probe.issue_with(&producers, &ClockCoincidence::of_pairs([(1, 2)])),
                Err(SameTickOrderError::Cycle { .. })
            ));
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
            let steps = issue_same_tick_schedule(
                probe.view,
                &probe.definitions,
                &producers,
                &excluded,
                &ClockCoincidence::never(),
            )
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
            let closure = probe
                .definitions
                .read_closure(probe.view, &[probe.read_of("nAlias")], &[])
                .unwrap();
            assert!(closure.contains(&probe.index("nAlias")));
            assert!(
                closure.contains(&probe.index("n")),
                "the closure of a read of `nAlias` must reach `n`"
            );
            let unrelated = probe
                .definitions
                .read_closure(probe.view, &[probe.read_of("b")], &[])
                .unwrap();
            assert_eq!(unrelated, BTreeSet::from([probe.index("b")]));
        });
    }
}
