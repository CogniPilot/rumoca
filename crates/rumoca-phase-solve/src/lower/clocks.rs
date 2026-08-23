use std::collections::{BTreeMap, BTreeSet, VecDeque};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::UnknownId;

use super::{LoweredLayout, StructuralMatching};
use crate::LowerError;

pub(super) struct LoweredClocks<'dae> {
    pub(super) partition: solve::SolveClockPartition,
    dae_clocks: Vec<solve::PeriodicClockId>,
    variable_owners: Vec<Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>>,
    sampled_variables: Vec<bool>,
    marker: std::marker::PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> LoweredClocks<'dae> {
    pub(super) fn clock(
        &self,
        clock: dae::ClockId<'dae>,
    ) -> Result<solve::PeriodicClockId, LowerError> {
        self.dae_clocks
            .get(clock.index() as usize)
            .copied()
            .ok_or_else(|| {
                LowerError::unspanned_non_computable(
                    "clock ownership refers outside the checked DAE clock arena",
                )
            })
    }

    pub(super) fn clock_index(&self, index: usize) -> Result<solve::PeriodicClockId, LowerError> {
        self.dae_clocks.get(index).copied().ok_or_else(|| {
            LowerError::unspanned_non_computable(
                "clock ownership refers outside the checked DAE clock arena",
            )
        })
    }

    pub(super) fn variable_owner(
        &self,
        variable: dae::VariableId<'dae>,
    ) -> Option<(dae::ClockId<'dae>, solve::PeriodicClockId)> {
        self.variable_owners
            .get(variable.index() as usize)
            .copied()
            .flatten()
    }

    pub(super) fn variable_is_sampled(&self, variable: dae::VariableId<'dae>) -> bool {
        self.sampled_variables
            .get(variable.index() as usize)
            .copied()
            .unwrap_or(false)
    }
}

pub(super) fn lower_clocks<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
) -> Result<LoweredClocks<'dae>, LowerError> {
    let mut partition = solve::SolveClockPartition {
        periodic_event_schedules: Vec::with_capacity(view.clock_count()),
        activation_parameter_indices: layout.clock_activations.clone(),
    };
    let mut dae_clocks = Vec::with_capacity(view.clock_count());
    for (index, (_, clock)) in view.clocks().enumerate() {
        let dae::ClockOperation::Periodic(schedule) = clock.operation() else {
            return Err(LowerError::unsupported(
                "triggered clocks do not yet have checked Solve scheduling",
                clock.provenance().span(),
            ));
        };
        let schedule = solve::PeriodicEventSchedule::from_schedule(*schedule).map_err(|error| {
            LowerError::contract(
                format!("checked DAE clock lattice cannot form a Solve schedule: {error}"),
                clock.provenance().span(),
            )
        })?;
        partition.periodic_event_schedules.push(schedule);
        let solve_clock = partition
            .periodic_clock_id(index)
            .expect("u32 checked DAE clock identity fits Solve clock identity");
        dae_clocks.push(solve_clock);
    }

    let mut variable_owners = vec![None; view.variable_count()];
    let mut sampled_variables = vec![false; view.variable_count()];
    for (_, ownership) in view.clock_ownerships() {
        let solve_clock = dae_clocks[ownership.clock().index() as usize];
        let slot = &mut variable_owners[ownership.variable().index() as usize];
        if slot.replace((ownership.clock(), solve_clock)).is_some() {
            return Err(LowerError::contract(
                "checked DAE variable has more than one clock owner",
                ownership.provenance().span(),
            ));
        }
        sampled_variables[ownership.variable().index() as usize] = ownership.sampled();
    }

    Ok(LoweredClocks {
        partition,
        dae_clocks,
        variable_owners,
        sampled_variables,
        marker: std::marker::PhantomData,
    })
}

/// Acceptance contract for the schedule a clocked partition may express
/// (MLS §16.5.1 `sample`, §16.5.2 `hold`).
///
/// MLS defines `sample(u)` as the *left limit* of its continuous-time operand:
/// `y(t_i) = u(t_i - eps)`, "the value of u just before the clock became
/// active", and states in the same paragraph that "algebraic loops between
/// clocked and continuous-time partitions cannot occur" precisely *because* of
/// that infinitesimal delay. A checked sampled owner is lowered against the
/// event-entry snapshot of `u`, so a discontinuity coincident with the clock
/// cannot replace the required left limit with the event's settled value.
///
/// **Accepted** — the typed sampled owner supplies the left-limit boundary, so
/// no schedule is invented:
/// * a clocked row whose continuous-time operands are outside the
///   *instantaneous* algebraic reach of the variables its own clock writes.
///   Every path that crosses a state coordinate is outside that reach: the
///   integrator, not the tick, determines the sampled value, so a plant driven
///   by `hold(..)` and sampled back through its states stays legal;
/// * a clocked row reading only its own partition's clocked variables,
///   `previous(..)`, `interval(..)`, parameters, inputs, and `time`.
///
/// **Rejected here for non-sampled clock owners** — this is the first owner
/// that sees a whole partition together with the continuous system it reads:
/// * a clocked row that reads a continuous-time variable causally reachable
///   through matched algebraic definitions from a variable the *same* clock
///   writes. Such a loop is not expressible under MLS §16.5.1; it only becomes
///   representable without the typed sampled-owner boundary, and the runtime
///   would otherwise settle it as an ordinary algebraic loop whose answer is
///   fixed by the loop gain instead of by the clock, silently replacing the
///   per-tick recurrence with a single steady-state solve.
///
/// Rejecting cross-clock `hold`/`sample` chains is deliberately *not* part of
/// this contract: those are not loops, and they keep their existing owner.
pub(super) fn reject_clocked_continuous_feedback<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    structural: &StructuralMatching<'dae>,
) -> Result<(), LowerError> {
    if view.variable_count() == 0 || view.clock_count() == 0 {
        return Ok(());
    }
    let definitions = collect_clocked_definitions(view, clocks)?;
    let dependencies = build_instantaneous_dependencies(view, structural, &definitions);
    for definition in &definitions {
        for operand in instantaneous_variables(view, [definition.value]) {
            if operand != definition.target
                && dependencies.reaches(definition.target, operand, definition.clock)
            {
                return Err(feedback_error(
                    view,
                    definition.target,
                    operand,
                    definition.clock,
                    definition.span,
                ));
            }
        }
    }
    Ok(())
}

/// Cross-partition companion to [`reject_clocked_continuous_feedback`].
///
/// The same-clock owner tags each clocked definition edge with its own clock
/// and follows only edges of that one clock, so it never sees a loop closed
/// through a *second* clock's definition. That blind spot is exactly the
/// coincident-tick case: two or more commensurate periodic clocks whose
/// producers read each other (through exact algebraic aliases, the only
/// DAE-admissible cross-partition read) at a tick where they all fire. On such
/// a tick every producer's row is issued together, so `a` reading `b` reading
/// back `a` is a discrete algebraic loop with no schedule.
///
/// Only genuine same-tick loops are rejected, never a blanket cross-clock ban:
/// * `pre(..)`/`previous(..)`/state reads carry a value from a strictly earlier
///   instant and are excluded by [`instantaneous_variables`], so a producer
///   that reads `pre(other)` breaks the cycle and is admitted;
/// * a cross-read that is not cyclic contributes no returning path and is
///   admitted;
/// * clocks that never share a tick (incommensurate periods, or the same period
///   at an offset phase) can never issue their rows together, so their rows are
///   never placed in one dependency graph here.
///
/// Two same-anchor periodic clocks share a tick exactly when the difference of
/// their phases is an integer multiple of the gcd of their periods
/// ([`clocks_share_a_tick`]); a set of clocks fires together exactly when every
/// pair in it shares a tick (generalized CRT), so the coincident firing sets
/// are the maximal cliques of that pairwise relation.
pub(super) fn reject_cross_clock_coincident_cycle<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    structural: &StructuralMatching<'dae>,
) -> Result<(), LowerError> {
    if view.variable_count() == 0 || view.clock_count() < 2 {
        return Ok(());
    }
    let definitions = collect_clocked_definitions(view, clocks)?;
    let active_clocks: BTreeSet<usize> = definitions.iter().map(|def| def.clock).collect();
    if active_clocks.len() < 2 {
        return Ok(());
    }
    let dependencies = build_instantaneous_dependencies(view, structural, &definitions);
    for clique in coincident_clock_cliques(&clocks.partition, &active_clocks) {
        if clique.len() < 2 {
            continue;
        }
        if let Some(error) = first_cross_clock_loop(view, &definitions, &dependencies, &clique) {
            return Err(error);
        }
    }
    Ok(())
}

/// The first same-tick loop closed among the definitions owned by `clique`, or
/// `None` if every one of them reaches only outside the clique.
///
/// A definition `target := value` closes a loop when some same-instant operand
/// of `value` is reachable back from `target` using only continuous edges and
/// the clocked edges of `clique` (the clocks that fire on the coincident tick).
fn first_cross_clock_loop<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &[ClockedDefinition<'dae>],
    dependencies: &InstantaneousDependencies,
    clique: &BTreeSet<usize>,
) -> Option<LowerError> {
    for definition in definitions {
        if !clique.contains(&definition.clock) {
            continue;
        }
        for operand in instantaneous_variables(view, [definition.value]) {
            if operand != definition.target
                && dependencies.reaches_within_clocks(definition.target, operand, clique)
            {
                return Some(cross_clock_loop_error(
                    view,
                    definition.target,
                    operand,
                    definition.span,
                ));
            }
        }
    }
    None
}

/// One clocked definition `target := value` owned by periodic clock `clock`.
///
/// Sampled owners are excluded: their left-limit boundary keeps them out of the
/// same-tick reach entirely (MLS §16.5.1).
struct ClockedDefinition<'dae> {
    target: usize,
    value: dae::ExprId<'dae>,
    clock: usize,
    span: rumoca_core::Span,
}

/// Collect every non-sampled clocked definition (B.1b discrete Reals and B.1c
/// discrete values) with the periodic clock that owns it.
fn collect_clocked_definitions<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<Vec<ClockedDefinition<'dae>>, LowerError> {
    let real_definitions = super::events::resolve_discrete_real_definitions(view)?;
    let mut rows = Vec::new();
    for (definition, equation) in real_definitions
        .into_iter()
        .zip(view.discrete_real_equations())
    {
        let Some((target, value)) = definition else {
            continue;
        };
        rows.push((
            dae::VariableId::from(target),
            value,
            equation.provenance().span(),
        ));
    }
    for index in 0..view.discrete_value_owner_count() {
        let id = view
            .discrete_value_owner_id(index)
            .expect("dense checked B.1c owner identity resolves");
        let owner = view
            .discrete_value_owner(id)
            .expect("checked B.1c owner resolves");
        for branch in owner.branches().iter() {
            for (target, (value, provenance)) in owner.targets().iter().zip(branch.values().iter())
            {
                rows.push((dae::VariableId::from(target), value, provenance.span()));
            }
        }
    }
    let mut definitions = Vec::new();
    for (target, value, span) in rows {
        if clocks.variable_is_sampled(target) {
            continue;
        }
        let Some(clock) = clocks
            .variable_owner(target)
            .map(|(_, clock)| clock.index())
        else {
            continue;
        };
        definitions.push(ClockedDefinition {
            target: target.index() as usize,
            value,
            clock,
            span,
        });
    }
    Ok(definitions)
}

/// Build the same-instant dependency graph of the continuous system and overlay
/// every clocked definition's edges, each tagged with its owning clock.
fn build_instantaneous_dependencies<'dae>(
    view: dae::DaeView<'dae>,
    structural: &StructuralMatching<'dae>,
    definitions: &[ClockedDefinition<'dae>],
) -> InstantaneousDependencies {
    let mut dependencies = InstantaneousDependencies::of_continuous_system(view, structural);
    for definition in definitions {
        dependencies.add_definition(view, definition.target, definition.value, definition.clock);
    }
    dependencies
}

fn feedback_error(
    view: dae::DaeView<'_>,
    target: usize,
    operand: usize,
    clock: usize,
    span: rumoca_core::Span,
) -> LowerError {
    LowerError::unsupported(
        format!(
            "periodic clock {clock} definition of `{}` reads `{}`, which is causally reachable \
             from that same target during the tick; MLS 16.5.1 gives an explicit sample(u) its \
             left-limit delay, but this ordinary periodic definition has no such boundary",
            variable_name(view, target),
            variable_name(view, operand)
        ),
        span,
    )
}

fn cross_clock_loop_error(
    view: dae::DaeView<'_>,
    target: usize,
    operand: usize,
    span: rumoca_core::Span,
) -> LowerError {
    LowerError::unsupported(
        format!(
            "coincident periodic clocks form a discrete same-tick algebraic loop: the clocked \
             definition of `{}` reads `{}`, whose own clocked definition is reachable back to \
             `{}` at a tick where the clocks fire together, following exact algebraic aliases; \
             MLS 16.5.1 gives an explicit sample(u) its left-limit delay, but these ordinary \
             periodic definitions across coincident clocks have no such boundary",
            variable_name(view, target),
            variable_name(view, operand),
            variable_name(view, target)
        ),
        span,
    )
}

fn variable_name(view: dae::DaeView<'_>, index: usize) -> String {
    view.variable_id(index)
        .and_then(|id| view.variable(id))
        .map(|variable| variable.name().to_string())
        .unwrap_or_else(|| "<unknown>".to_string())
}

/// The maximal sets of clocks that fire on a common tick, over `active_clocks`.
///
/// A set of same-anchor periodic clocks is jointly coincident exactly when it is
/// pairwise coincident (generalized CRT for the congruences `t ≡ phase_i (mod
/// period_i)`), so the coincident firing sets are the maximal cliques of the
/// pairwise "share a tick" relation.
fn coincident_clock_cliques(
    partition: &solve::SolveClockPartition,
    active_clocks: &BTreeSet<usize>,
) -> Vec<BTreeSet<usize>> {
    let clocks: Vec<usize> = active_clocks.iter().copied().collect();
    let mut adjacency: BTreeMap<usize, BTreeSet<usize>> = clocks
        .iter()
        .map(|&clock| (clock, BTreeSet::new()))
        .collect();
    for (position, &left) in clocks.iter().enumerate() {
        for &right in &clocks[position + 1..] {
            if clocks_share_a_tick(partition, left, right) {
                adjacency.entry(left).or_default().insert(right);
                adjacency.entry(right).or_default().insert(left);
            }
        }
    }
    let mut cliques = Vec::new();
    bron_kerbosch(
        BTreeSet::new(),
        active_clocks.clone(),
        BTreeSet::new(),
        &adjacency,
        &mut cliques,
    );
    cliques
}

/// Bron-Kerbosch maximal-clique enumeration over the small coincidence graph.
fn bron_kerbosch(
    chosen: BTreeSet<usize>,
    mut candidates: BTreeSet<usize>,
    mut excluded: BTreeSet<usize>,
    adjacency: &BTreeMap<usize, BTreeSet<usize>>,
    cliques: &mut Vec<BTreeSet<usize>>,
) {
    if candidates.is_empty() && excluded.is_empty() {
        cliques.push(chosen);
        return;
    }
    let empty = BTreeSet::new();
    for vertex in candidates.iter().copied().collect::<Vec<_>>() {
        let neighbors = adjacency.get(&vertex).unwrap_or(&empty);
        let mut next_chosen = chosen.clone();
        next_chosen.insert(vertex);
        let next_candidates = candidates.intersection(neighbors).copied().collect();
        let next_excluded = excluded.intersection(neighbors).copied().collect();
        bron_kerbosch(
            next_chosen,
            next_candidates,
            next_excluded,
            adjacency,
            cliques,
        );
        candidates.remove(&vertex);
        excluded.insert(vertex);
    }
}

/// Whether two periodic clocks ever tick at the same instant.
///
/// Clocks anchored differently (absolute versus simulation-start) cannot be
/// compared without the resolved start instant, so they are treated as never
/// coincident here rather than rejected on an unproven overlap. Same-anchor
/// clocks share a tick when `phase_b - phase_a` is an integer multiple of the
/// gcd of their periods; a phase difference of zero (the common phase-aligned
/// case) is always such a multiple.
fn clocks_share_a_tick(partition: &solve::SolveClockPartition, left: usize, right: usize) -> bool {
    let (Some(a), Some(b)) = (
        partition.periodic_event_schedules.get(left),
        partition.periodic_event_schedules.get(right),
    ) else {
        return false;
    };
    if a.anchor() != b.anchor() {
        return false;
    }
    let (lattice_a, lattice_b) = (a.lattice(), b.lattice());
    let Ok(phase_delta) = lattice_b.phase().checked_sub(lattice_a.phase()) else {
        return false;
    };
    if phase_delta.is_zero() {
        return true;
    }
    let Some(period_gcd) = rational_gcd(lattice_a.period(), lattice_b.period()) else {
        return false;
    };
    match phase_delta.checked_div(period_gcd) {
        Ok(ratio) => ratio.denominator() == 1,
        Err(_) => false,
    }
}

/// The gcd of two positive rationals: the generator of the additive subgroup
/// they span, `gcd(numerators over a common denominator) / that denominator`.
fn rational_gcd(
    left: rumoca_core::ClockRational,
    right: rumoca_core::ClockRational,
) -> Option<rumoca_core::ClockRational> {
    let (left_denominator, right_denominator) = (left.denominator(), right.denominator());
    let denominator_gcd = gcd_i128(left_denominator, right_denominator);
    let common_denominator = (left_denominator / denominator_gcd).checked_mul(right_denominator)?;
    let left_numerator = left
        .numerator()
        .checked_mul(right_denominator / denominator_gcd)?;
    let right_numerator = right
        .numerator()
        .checked_mul(left_denominator / denominator_gcd)?;
    let numerator_gcd = gcd_i128(left_numerator, right_numerator);
    rumoca_core::ClockRational::new(numerator_gcd, common_denominator).ok()
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    left = left.abs();
    right = right.abs();
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }
    left
}

/// Variables an expression set reads *at the same instant*.
///
/// State and derivative coordinates are deliberately excluded. A state's value
/// is produced by integration, not by the equation it appears in, so it
/// separates a tick from anything the tick writes; a derivative coordinate is
/// never a sampled value. `pre`/`previous` coordinates are excluded for the
/// same reason: they carry a value from a strictly earlier instant.
fn instantaneous_variables<'dae>(
    view: dae::DaeView<'dae>,
    roots: impl IntoIterator<Item = dae::ExprId<'dae>>,
) -> BTreeSet<usize> {
    let mut variables = BTreeSet::new();
    for root in roots {
        dae::for_each_expression(view, root, |_, expression| {
            if let dae::ExpressionOperation::Coordinate(coordinate) = expression.operation()
                && let Some(variable) = instantaneous_coordinate_variable(coordinate)
            {
                variables.insert(variable as usize);
            }
        });
    }
    variables
}

fn instantaneous_coordinate_variable(coordinate: dae::CoordinateView<'_>) -> Option<u32> {
    match coordinate {
        dae::CoordinateView::Algebraic(id) => Some(id.index()),
        dae::CoordinateView::DiscreteReal(id) => Some(id.index()),
        dae::CoordinateView::DiscreteValue(id) => Some(id.index()),
        dae::CoordinateView::State(_)
        | dae::CoordinateView::Derivative(_)
        | dae::CoordinateView::Parameter(_)
        | dae::CoordinateView::Input(_)
        | dae::CoordinateView::Time
        | dae::CoordinateView::ClockInterval(_)
        | dae::CoordinateView::PreDiscreteReal(_)
        | dae::CoordinateView::PreDiscreteValue(_)
        | dae::CoordinateView::PreState(_)
        | dae::CoordinateView::PreAlgebraic(_)
        | dae::CoordinateView::Condition(_)
        | dae::CoordinateView::Delay(_)
        | dae::CoordinateView::Previous(_)
        | dae::CoordinateView::Terminal(_)
        | dae::CoordinateView::Binder(_)
        | dae::CoordinateView::FunctionParameter(_) => None,
    }
}

/// Directed same-instant dependencies proved by structural matching.
///
/// An algebraic equation contributes edges from every coordinate it reads to
/// the algebraic coordinate structural analysis matched as its result. A row
/// matched to a state derivative contributes no edge: integration, rather than
/// algebraic evaluation at the tick, produces the state reached through it.
struct InstantaneousDependencies {
    successors: Vec<BTreeSet<DependencyEdge>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum DependencyEdge {
    Continuous(usize),
    Clocked { clock: usize, target: usize },
}

impl InstantaneousDependencies {
    fn of_continuous_system<'dae>(
        view: dae::DaeView<'dae>,
        structural: &StructuralMatching<'dae>,
    ) -> Self {
        let mut dependencies = Self {
            successors: vec![BTreeSet::new(); view.variable_count()],
        };
        let mut row = 0usize;
        for owner in view.continuous_owners() {
            row = dependencies.add_owner_rows(view, owner, row, structural);
        }
        debug_assert_eq!(row, structural.rows.len());
        dependencies
    }

    fn add_owner_rows<'dae>(
        &mut self,
        view: dae::DaeView<'dae>,
        owner: dae::ContinuousOwnerView<'dae>,
        first_row: usize,
        structural: &StructuralMatching<'dae>,
    ) -> usize {
        let (variables, row_count) = match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                let residual = equation.residual();
                let variables = instantaneous_variables(view, [residual]);
                let row_count = view
                    .expression(residual)
                    .expect("checked residual expression resolves")
                    .value_type()
                    .scalar_count()
                    .expect("checked residual scalar capacity");
                (variables, row_count)
            }
            dae::ContinuousOwnerView::Structured { family, .. } => (
                instantaneous_variables(view, family.bodies().iter()),
                usize::try_from(family.scalar_rows())
                    .expect("checked structured row capacity fits the host"),
            ),
        };
        let end = first_row + row_count;
        for row in first_row..end {
            self.add_row(row, &variables, structural);
        }
        end
    }

    fn add_row(
        &mut self,
        row: usize,
        variables: &BTreeSet<usize>,
        structural: &StructuralMatching<'_>,
    ) {
        let Some(UnknownId::Algebraic { variable, .. }) = structural.rows.get(&row) else {
            return;
        };
        let target = variable.index() as usize;
        for source in variables.iter().copied().filter(|source| *source != target) {
            self.successors[source].insert(DependencyEdge::Continuous(target));
        }
    }

    fn add_definition<'dae>(
        &mut self,
        view: dae::DaeView<'dae>,
        target: usize,
        value: dae::ExprId<'dae>,
        clock: usize,
    ) {
        for source in instantaneous_variables(view, [value]) {
            if source != target {
                self.successors[source].insert(DependencyEdge::Clocked { clock, target });
            }
        }
    }

    fn reaches(&self, start: usize, goal: usize, clock: usize) -> bool {
        let mut visited = vec![false; self.successors.len()];
        let mut frontier = VecDeque::from([start]);
        visited[start] = true;
        while let Some(variable) = frontier.pop_front() {
            if self.enqueue_successors(variable, goal, clock, &mut visited, &mut frontier) {
                return true;
            }
        }
        false
    }

    fn enqueue_successors(
        &self,
        variable: usize,
        goal: usize,
        clock: usize,
        visited: &mut [bool],
        frontier: &mut VecDeque<usize>,
    ) -> bool {
        for edge in self.successors[variable].iter().copied() {
            let Some(successor) = edge.target_on_clock(clock) else {
                continue;
            };
            if successor == goal {
                return true;
            }
            if visited[successor] {
                continue;
            }
            visited[successor] = true;
            frontier.push_back(successor);
        }
        false
    }

    /// Same-instant reachability using continuous edges plus the clocked edges
    /// of any clock in `allowed` (a coincident firing set), rather than the
    /// single owning clock of [`Self::reaches`].
    fn reaches_within_clocks(&self, start: usize, goal: usize, allowed: &BTreeSet<usize>) -> bool {
        let mut visited = vec![false; self.successors.len()];
        let mut frontier = VecDeque::from([start]);
        visited[start] = true;
        while let Some(variable) = frontier.pop_front() {
            if self.enqueue_successors_within_clocks(
                variable,
                goal,
                allowed,
                &mut visited,
                &mut frontier,
            ) {
                return true;
            }
        }
        false
    }

    fn enqueue_successors_within_clocks(
        &self,
        variable: usize,
        goal: usize,
        allowed: &BTreeSet<usize>,
        visited: &mut [bool],
        frontier: &mut VecDeque<usize>,
    ) -> bool {
        for edge in self.successors[variable].iter().copied() {
            let Some(successor) = edge.target_in_clocks(allowed) else {
                continue;
            };
            if successor == goal {
                return true;
            }
            if visited[successor] {
                continue;
            }
            visited[successor] = true;
            frontier.push_back(successor);
        }
        false
    }
}

impl DependencyEdge {
    fn target_on_clock(self, clock: usize) -> Option<usize> {
        match self {
            Self::Continuous(target) => Some(target),
            Self::Clocked {
                clock: owner,
                target,
            } if owner == clock => Some(target),
            Self::Clocked { .. } => None,
        }
    }

    fn target_in_clocks(self, allowed: &BTreeSet<usize>) -> Option<usize> {
        match self {
            Self::Continuous(target) => Some(target),
            Self::Clocked { clock, target } if allowed.contains(&clock) => Some(target),
            Self::Clocked { .. } => None,
        }
    }
}
