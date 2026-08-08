use std::collections::{BTreeSet, VecDeque};

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
    for index in 0..view.clock_count() {
        let dae_clock = view
            .clock_id(index)
            .expect("dense checked clock identity resolves");
        let clock = view
            .clock(dae_clock)
            .expect("checked clock identity resolves");
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
    for index in 0..view.clock_ownership_count() {
        let ownership = view
            .clock_ownership_id(index)
            .and_then(|id| view.clock_ownership(id))
            .expect("dense checked clock ownership resolves");
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
    let real_definitions = super::events::resolve_discrete_real_definitions(view)?;
    let mut rows = Vec::new();
    for (index, (target, value)) in real_definitions.into_iter().enumerate() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
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
    let mut dependencies = InstantaneousDependencies::of_continuous_system(view, structural);
    for &(target, value, _) in &rows {
        if clocks.variable_is_sampled(target) {
            continue;
        }
        if let Some(clock) = clocks
            .variable_owner(target)
            .map(|(_, clock)| clock.index())
        {
            dependencies.add_definition(view, target.index() as usize, value, clock);
        }
    }
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
        let target_index = target.index() as usize;
        for operand in instantaneous_variables(view, [value]) {
            if operand != target_index && dependencies.reaches(target_index, operand, clock) {
                return Err(feedback_error(view, target_index, operand, clock, span));
            }
        }
    }
    Ok(())
}

fn feedback_error(
    view: dae::DaeView<'_>,
    target: usize,
    operand: usize,
    clock: usize,
    span: rumoca_core::Span,
) -> LowerError {
    let name = |index| {
        view.variable_id(index)
            .and_then(|id| view.variable(id))
            .map(|variable| variable.name().to_string())
            .unwrap_or_else(|| "<unknown>".to_string())
    };
    LowerError::unsupported(
        format!(
            "periodic clock {clock} definition of `{}` reads `{}`, which is causally reachable \
             from that same target during the tick; MLS 16.5.1 gives an explicit sample(u) its \
             left-limit delay, but this ordinary periodic definition has no such boundary",
            name(target),
            name(operand)
        ),
        span,
    )
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
}
