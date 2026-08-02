use std::collections::{BTreeSet, HashMap};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::LoweredLayout;
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
/// * a clocked row that reads a continuous-time variable which is
///   instantaneously (algebraically) coupled to a variable the *same* clock
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
) -> Result<(), LowerError> {
    let variable_count = view.variable_count();
    if variable_count == 0 || view.clock_count() == 0 {
        return Ok(());
    }
    let reach = InstantaneousReach::of_continuous_system(view, variable_count);
    let written = clocked_reach_roots(view, clocks, &reach, variable_count);
    if written.is_empty() {
        return Ok(());
    }
    for index in 0..view.discrete_real_equation_count() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        if expression_defines_sampled_variable(view, clocks, equation.residual()) {
            continue;
        }
        check_clocked_row_operands(
            view,
            clocks,
            &reach,
            &written,
            equation.residual(),
            equation.provenance().span(),
        )?;
    }
    for index in 0..view.discrete_value_owner_count() {
        let id = view
            .discrete_value_owner_id(index)
            .expect("dense checked B.1c owner identity resolves");
        let owner = view
            .discrete_value_owner(id)
            .expect("checked B.1c owner resolves");
        let values = owner.branches().iter().flat_map(|branch| {
            owner
                .targets()
                .iter()
                .zip(branch.values().iter())
                .filter(|(target, _)| !clocks.variable_is_sampled(dae::VariableId::from(*target)))
        });
        for (_, (value, provenance)) in values {
            check_clocked_row_operands(view, clocks, &reach, &written, value, provenance.span())?;
        }
    }
    Ok(())
}

fn expression_defines_sampled_variable<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    root: dae::ExprId<'dae>,
) -> bool {
    let mut sampled = false;
    dae::for_each_expression(view, root, |_, expression| {
        sampled |= expression
            .variable_coordinate()
            .is_some_and(|variable| clocks.variable_is_sampled(variable));
    });
    sampled
}

/// One clocked row: the operands it reads must not reach its own clock's writes
/// through the continuous system.
fn check_clocked_row_operands<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    reach: &InstantaneousReach,
    written: &HashMap<usize, BTreeSet<usize>>,
    root: dae::ExprId<'dae>,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let operands = instantaneous_variables(view, [root]);
    if operands.is_empty() {
        return Ok(());
    }
    let mut row_clocks = BTreeSet::new();
    let mut continuous = Vec::new();
    for variable in operands.iter().copied() {
        match variable_clock(view, clocks, variable) {
            Some(clock) => {
                row_clocks.insert(clock);
            }
            None => continuous.push(variable),
        }
    }
    for variable in continuous {
        let Some(owners) = written.get(&reach.root(variable)) else {
            continue;
        };
        if let Some(clock) = row_clocks.intersection(owners).next().copied() {
            return Err(LowerError::unsupported(
                format!(
                    "clocked partition of periodic clock {clock} samples a continuous-time value \
                     that the same tick recomputes; MLS 16.5.1 defines sample(u) as the left \
                     limit of u, which this identity read is not, so the partition has no \
                     checked schedule"
                ),
                span,
            ));
        }
    }
    Ok(())
}

/// Roots of the instantaneous components each periodic clock writes into.
fn clocked_reach_roots<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    reach: &InstantaneousReach,
    variable_count: usize,
) -> HashMap<usize, BTreeSet<usize>> {
    let mut written: HashMap<usize, BTreeSet<usize>> = HashMap::new();
    for index in 0..variable_count {
        let Some(clock) = view
            .variable_id(index)
            .and_then(|id| clocks.variable_owner(id))
            .map(|(_, clock)| clock.index())
        else {
            continue;
        };
        if !reach.is_coupled(index) {
            continue;
        }
        written.entry(reach.root(index)).or_default().insert(clock);
    }
    written
}

fn variable_clock<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    variable: usize,
) -> Option<usize> {
    view.variable_id(variable)
        .and_then(|id| clocks.variable_owner(id))
        .map(|(_, clock)| clock.index())
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

/// Instantaneous (same-tick) algebraic components of the continuous system.
///
/// Every continuous equation couples the variables it reads at one instant, so
/// the components of that relation are exactly the sets of variables that
/// cannot be separated by a clock tick. Variables that appear in no continuous
/// equation stay isolated and are reported as uncoupled.
struct InstantaneousReach {
    parent: Vec<usize>,
    coupled: Vec<bool>,
}

impl InstantaneousReach {
    fn of_continuous_system<'dae>(view: dae::DaeView<'dae>, variable_count: usize) -> Self {
        let mut reach = Self {
            parent: (0..variable_count).collect(),
            coupled: vec![false; variable_count],
        };
        for owner in view.continuous_owners() {
            match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => {
                    reach.couple(&instantaneous_variables(view, [equation.residual()]));
                }
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    reach.couple(&instantaneous_variables(view, family.bodies().iter()));
                }
            }
        }
        reach
    }

    fn couple(&mut self, variables: &BTreeSet<usize>) {
        let mut members = variables.iter().copied();
        let Some(first) = members.next() else {
            return;
        };
        self.coupled[first] = true;
        for member in members {
            self.coupled[member] = true;
            self.union(first, member);
        }
    }

    fn is_coupled(&self, variable: usize) -> bool {
        self.coupled.get(variable).copied().unwrap_or(false)
    }

    fn root(&self, mut variable: usize) -> usize {
        while self.parent[variable] != variable {
            variable = self.parent[variable];
        }
        variable
    }

    fn union(&mut self, left: usize, right: usize) {
        let left = self.root(left);
        let right = self.root(right);
        if left != right {
            self.parent[right] = left;
        }
    }
}
