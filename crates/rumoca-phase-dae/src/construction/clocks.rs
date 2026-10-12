use std::collections::HashMap;

use rumoca_core::{InstanceId, PeriodicClockSchedule, Span, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use super::Coordinate;
use super::analysis::{ClockPlan, ClockSchedule, ClockedValuePlan, EventClockPlan};

pub(super) struct LoweredClocks<'dae> {
    pub(super) by_plan: HashMap<ClockPlan, dae::ClockId<'dae>>,
    /// The exact lattice of every periodic clock, keyed by clock index; an
    /// event clock has none.
    lattices: HashMap<u32, (dae::PeriodicClockId<'dae>, rumoca_core::ClockLattice)>,
    /// MLS §3.7.5 event clocks are identified entirely by their exact
    /// periodic schedule. Every occurrence and exact Boolean alias reuses this
    /// one owner rather than allocating parallel activation lanes.
    by_sample_schedule: HashMap<PeriodicClockSchedule, dae::PeriodicClockId<'dae>>,
    /// Clock coordinates keyed by their Flat catalog name, the identity every
    /// Flat expression occurrence names. `Reference::instance_id` carries the
    /// enclosing class occurrence, so it cannot select a referenced coordinate.
    pub(super) by_coordinate: HashMap<VarName, dae::ClockId<'dae>>,
    /// MLS §16.10 `firstTick()`: per clock, the `previous` of a generated
    /// clocked indicator that starts at one and is zero after every tick.
    first_ticks: HashMap<dae::ClockId<'dae>, dae::PreviousId<'dae>>,
    /// MLS §16.3 event clocks whose tick conditions are reserved at clock
    /// lowering and defined once the condition coordinates exist.
    pending_events: Vec<(dae::ConditionId<'dae>, EventClockPlan)>,
    /// MLS §16.5.2 shifted event clocks whose tick conditions are reserved
    /// at clock lowering and defined once their base tick counters exist.
    pending_shifts: Vec<PendingShift<'dae>>,
    /// Per event clock with shifted clocks, the generated coordinate that
    /// counts its ticks.
    tick_counters: HashMap<dae::ClockId<'dae>, dae::DiscreteRealId<'dae>>,
    /// The constructor of every event clock.
    events: HashMap<dae::ClockId<'dae>, EventClockPlan>,
    /// MLS §16.10 `interval()` of an event clock: per clock, the `previous`
    /// of a generated clocked coordinate that holds the time of each tick.
    last_ticks: HashMap<dae::ClockId<'dae>, dae::PreviousId<'dae>>,
}

impl<'dae> LoweredClocks<'dae> {
    pub(super) fn id(
        &self,
        plan: &ClockPlan,
        span: rumoca_core::Span,
    ) -> Result<dae::ClockId<'dae>, dae::DaeConstructionError> {
        self.by_plan
            .get(plan)
            .copied()
            .ok_or(dae::DaeConstructionError::MissingClockDomainOwner { span })
    }

    /// The exact lattice of the lowered clock `id`; `None` for an event clock.
    pub(super) fn lattice(&self, id: dae::ClockId<'dae>) -> Option<rumoca_core::ClockLattice> {
        self.lattices.get(&id.index()).map(|(_, lattice)| *lattice)
    }

    /// The periodic identity of the lowered clock `id`; `None` for an event
    /// clock.
    pub(super) fn periodic(&self, id: dae::ClockId<'dae>) -> Option<dae::PeriodicClockId<'dae>> {
        self.lattices
            .get(&id.index())
            .map(|(periodic, _)| *periodic)
    }

    /// The `previous` coordinate whose value is `firstTick()` of `clock`.
    pub(super) fn first_tick(
        &self,
        clock: dae::ClockId<'dae>,
        span: Span,
    ) -> Result<dae::PreviousId<'dae>, dae::DaeConstructionError> {
        self.first_ticks
            .get(&clock)
            .copied()
            .ok_or(dae::DaeConstructionError::MissingClockDomainOwner { span })
    }

    pub(super) fn sample_id(
        &self,
        schedule: PeriodicClockSchedule,
        span: Span,
    ) -> Result<dae::ClockId<'dae>, dae::DaeConstructionError> {
        self.by_sample_schedule
            .get(&schedule)
            .copied()
            .map(Into::into)
            .ok_or(dae::DaeConstructionError::MissingClockDomainOwner { span })
    }

    /// Define the tick condition of every MLS §16.3 event clock: the clock
    /// ticks when `edge(pre(condition))` becomes true. Its checked pre
    /// coordinate stays fixed while the ordinary equations settle, so transient
    /// iterates cannot activate the clock. A settled rise ticks the clock one
    /// event iteration later, at the same instant.
    pub(super) fn define_event_conditions(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        coordinates: &HashMap<VarName, Coordinate<'dae>>,
    ) -> Result<(), dae::DaeConstructionError> {
        for (condition, event) in std::mem::take(&mut self.pending_events) {
            let provenance = dae::DaeProvenance::source(event.span)?;
            let tick = event_condition(construction, coordinates, &event)?;
            construction.conditions(|conditions| {
                conditions.define(condition, dae::ConditionInput::Discrete(tick), provenance)
            })?;
        }
        // MLS §16.5.2: a clock shifted by `skip` ticks of its base ticks with the
        // base once the base has counted more than `skip` ticks. The tick
        // programs of the shifted clock observe the base's tick counter at the
        // same tick, so both clocks tick in the same event iteration.
        for shift in std::mem::take(&mut self.pending_shifts) {
            let provenance = dae::DaeProvenance::source(shift.event.span)?;
            let counter = self.tick_counters.get(&shift.base).copied().ok_or(
                dae::DaeConstructionError::MissingClockDomainOwner {
                    span: shift.event.span,
                },
            )?;
            let tick = event_condition(construction, coordinates, &shift.event)?;
            let shifted = construction.expressions(|expressions| {
                let count = expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::DiscreteReal(counter))?;
                let skipped = expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(f64::from(shift.skip) + 0.5))?;
                let past = expressions.at(provenance).binary(
                    dae::BinaryOperator::Greater,
                    count,
                    skipped,
                )?;
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::And, tick, past)
            })?;
            construction.conditions(|conditions| {
                conditions.define(
                    shift.condition,
                    dae::ConditionInput::Discrete(shifted),
                    provenance,
                )
            })?;
        }
        Ok(())
    }
}

/// The Boolean coordinate an event clock's condition names, read at the tick.
fn event_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    event: &EventClockPlan,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(event.span)?;
    let Some(Coordinate::DiscreteValue(variable)) = coordinates.get(&event.condition).copied()
    else {
        return Err(dae::DaeConstructionError::InvalidVariableRole {
            name: event.condition.clone(),
            span: event.span,
        });
    };
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::PreDiscreteValue(variable))
    })
}

pub(super) fn lower_clocks<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    events: &HashMap<InstanceId, EventClockPlan>,
    clocked_values: &HashMap<InstanceId, ClockedValuePlan>,
    sample_schedules: impl Iterator<Item = (PeriodicClockSchedule, Span)>,
) -> Result<LoweredClocks<'dae>, dae::DaeConstructionError> {
    let mut lowered = LoweredClocks {
        by_plan: HashMap::new(),
        lattices: HashMap::new(),
        by_sample_schedule: HashMap::new(),
        by_coordinate: HashMap::new(),
        first_ticks: HashMap::new(),
        pending_events: Vec::new(),
        pending_shifts: Vec::new(),
        tick_counters: HashMap::new(),
        events: HashMap::new(),
        last_ticks: HashMap::new(),
    };
    for (name, variable) in &flat.variables {
        let Some(plan) = plans.get(&variable.instance_id).copied() else {
            continue;
        };
        let clock = lowered.issue(construction, plan, events)?;
        lowered.by_coordinate.insert(name.clone(), clock);
    }
    for (_, value) in clocked_values_in_instance_order(clocked_values) {
        lowered.issue(construction, value.clock, events)?;
    }
    for (schedule, span) in sample_schedules {
        if lowered.by_sample_schedule.contains_key(&schedule) {
            continue;
        }
        let provenance = dae::DaeProvenance::source(span)?;
        let clock = construction.clocks(|clocks| clocks.scheduled(schedule, provenance))?;
        lowered.by_sample_schedule.insert(schedule, clock);
    }
    Ok(lowered)
}

impl<'dae> LoweredClocks<'dae> {
    /// The DAE clock of `plan`, issued on its first use.
    fn issue(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        plan: ClockPlan,
        events: &HashMap<InstanceId, EventClockPlan>,
    ) -> Result<dae::ClockId<'dae>, dae::DaeConstructionError> {
        if let Some(clock) = self.by_plan.get(&plan).copied() {
            return Ok(clock);
        }
        let provenance = dae::DaeProvenance::source(plan.constructor_span)?;
        let clock = match plan.schedule {
            ClockSchedule::Periodic(lattice) => {
                let periodic =
                    construction.clocks(|clocks| clocks.periodic(lattice, provenance))?;
                self.lattices.insert(periodic.index(), (periodic, lattice));
                periodic.into()
            }
            ClockSchedule::Event { source, skip } => {
                let event = events.get(&source).cloned().ok_or(
                    dae::DaeConstructionError::MissingClockDomainOwner {
                        span: plan.constructor_span,
                    },
                )?;
                let condition =
                    construction.conditions(|conditions| conditions.reserve(provenance))?;
                let clock = if skip == 0 {
                    self.pending_events.push((condition, event.clone()));
                    construction.clocks(|clocks| clocks.triggered(condition, provenance))?
                } else {
                    let base_plan = ClockPlan {
                        schedule: ClockSchedule::Event { source, skip: 0 },
                        constructor_span: plan.constructor_span,
                    };
                    let base = self.issue(construction, base_plan, events)?;
                    self.pending_shifts.push(PendingShift {
                        condition,
                        base,
                        skip,
                        event: event.clone(),
                    });
                    construction
                        .clocks(|clocks| clocks.shifted(base, skip, condition, provenance))?
                };
                self.events.insert(clock, event);
                clock
            }
        };
        self.by_plan.insert(plan, clock);
        Ok(clock)
    }

    /// Issue the `firstTick()` indicator of every clock whose partition reads
    /// it (MLS §16.10: true at the first tick of the clock, false afterwards).
    ///
    /// The indicator is a generated clocked discrete Real `f` with `start =
    /// 1` and the partition equation `f = 0`, so `previous(f)` is one exactly
    /// at the first tick; `firstTick()` lowers to `previous(f) > 0.5`.
    pub(super) fn issue_first_ticks(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        hosts: impl IntoIterator<Item = (ClockPlan, Span)>,
    ) -> Result<(), dae::DaeConstructionError> {
        for (clock, span) in self.host_clocks(hosts)? {
            let (_, previous) = issue_clocked_indicator(
                construction,
                ClockedIndicator {
                    clock,
                    span,
                    name: format!("firstTick(clock {})", clock.index()),
                    start: 1.0,
                    value: IndicatorValue::Zero,
                },
            )?;
            self.first_ticks.insert(clock, previous);
        }
        Ok(())
    }

    /// Issue the tick counter of every event clock that a shifted clock
    /// (MLS §16.5.2 `shiftSample`) skips ticks of: a generated clocked discrete
    /// Real `n` with `start = 0` and the partition equation
    /// `n = previous(n) + 1`, so `n` is the number of ticks so far.
    pub(super) fn issue_tick_counters(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        let mut bases = self
            .pending_shifts
            .iter()
            .map(|shift| (shift.base, shift.event.span))
            .collect::<Vec<_>>();
        bases.sort_by_key(|(clock, _)| clock.index());
        bases.dedup_by_key(|(clock, _)| *clock);
        for (clock, span) in bases {
            let (counter, _) = issue_clocked_indicator(
                construction,
                ClockedIndicator {
                    clock,
                    span,
                    name: format!("tickCount(clock {})", clock.index()),
                    start: 0.0,
                    value: IndicatorValue::Count,
                },
            )?;
            self.tick_counters.insert(clock, counter);
        }
        Ok(())
    }

    /// Issue the tick-time coordinate of every event clock whose partition
    /// reads `interval()` (MLS §16.10 Operator 16.15): a generated clocked
    /// discrete Real `l` with the partition equation `l = time`, so
    /// `previous(l)` is the time of the previous tick.
    pub(super) fn issue_event_intervals(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        hosts: impl IntoIterator<Item = (ClockPlan, Span)>,
    ) -> Result<(), dae::DaeConstructionError> {
        for (clock, span) in self.host_clocks(hosts)? {
            let (_, previous) = issue_clocked_indicator(
                construction,
                ClockedIndicator {
                    clock,
                    span,
                    name: format!("lastTick(clock {})", clock.index()),
                    start: 0.0,
                    value: IndicatorValue::Time,
                },
            )?;
            self.last_ticks.insert(clock, previous);
        }
        Ok(())
    }

    /// The parts of `interval()` of the event clock `clock`: the `previous`
    /// of its first-tick indicator and of its tick time, and its
    /// `startInterval` (`None` is the default 0).
    pub(super) fn event_interval(
        &self,
        clock: dae::ClockId<'dae>,
        span: Span,
    ) -> Result<EventInterval<'_, 'dae>, dae::DaeConstructionError> {
        let missing = dae::DaeConstructionError::MissingClockDomainOwner { span };
        Ok(EventInterval {
            first_tick: self.first_tick(clock, span)?,
            last_tick: self
                .last_ticks
                .get(&clock)
                .copied()
                .ok_or(missing.clone())?,
            start_interval: self
                .events
                .get(&clock)
                .ok_or(missing)?
                .start_interval
                .as_ref(),
        })
    }

    fn host_clocks(
        &self,
        hosts: impl IntoIterator<Item = (ClockPlan, Span)>,
    ) -> Result<Vec<(dae::ClockId<'dae>, Span)>, dae::DaeConstructionError> {
        let mut clocks = hosts
            .into_iter()
            .map(|(plan, span)| self.id(&plan, span).map(|clock| (clock, span)))
            .collect::<Result<Vec<_>, _>>()?;
        clocks.sort_by_key(|(clock, _)| clock.index());
        clocks.dedup_by_key(|(clock, _)| *clock);
        Ok(clocks)
    }
}

/// The parts `interval()` of an event clock is built from.
pub(super) struct EventInterval<'plan, 'dae> {
    pub(super) first_tick: dae::PreviousId<'dae>,
    pub(super) last_tick: dae::PreviousId<'dae>,
    pub(super) start_interval: Option<&'plan rumoca_core::Expression>,
}

/// A shifted event clock whose tick condition is defined once the tick counter
/// of its base exists.
struct PendingShift<'dae> {
    condition: dae::ConditionId<'dae>,
    base: dae::ClockId<'dae>,
    skip: u32,
    event: EventClockPlan,
}

/// The value a generated clocked indicator takes at every tick.
enum IndicatorValue {
    Zero,
    Time,
    /// One more than its own previous value: the number of ticks so far.
    Count,
}

struct ClockedIndicator<'dae> {
    clock: dae::ClockId<'dae>,
    span: Span,
    name: String,
    start: f64,
    value: IndicatorValue,
}

/// A generated clocked discrete Real owned by `indicator.clock`, starting at
/// `indicator.start` and set to its value at every tick; returns it and its
/// `previous`.
fn issue_clocked_indicator<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    indicator: ClockedIndicator<'dae>,
) -> Result<(dae::DiscreteRealId<'dae>, dae::PreviousId<'dae>), dae::DaeConstructionError> {
    let ClockedIndicator {
        clock,
        span,
        name,
        start,
        value,
    } = indicator;
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ClockLowering, span)?;
    let value_type = construction
        .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance))?;
    let start = construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Real(start))
    })?;
    let variable = construction.variables(|variables| {
        variables.discrete_real(
            VarName::new(name),
            value_type,
            provenance,
            dae::VariableAttributes {
                start: Some(start),
                fixed: Some(vec![true]),
                origin: dae::VariableOrigin::Generated,
                ..Default::default()
            },
        )
    })?;
    construction.clocks(|clocks| {
        clocks.own_discrete_real(clock, variable, provenance)?;
        Ok(())
    })?;
    let previous = construction
        .temporal(|temporal| temporal.previous_discrete_real(clock, variable, provenance))?;
    let residual = construction.expressions(|expressions| {
        let current = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
        let value = match value {
            IndicatorValue::Zero => expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(0.0))?,
            IndicatorValue::Time => expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Time)?,
            IndicatorValue::Count => {
                let last = expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::Previous(previous))?;
                let one = expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(1.0))?;
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Add, last, one)?
            }
        };
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, current, value)
    })?;
    construction.discrete(|system| {
        system.real_equation(provenance, |equation| equation.residual(residual))
    })?;
    Ok((variable, previous))
}

fn clocked_values_in_instance_order(
    values: &HashMap<InstanceId, ClockedValuePlan>,
) -> Vec<(InstanceId, &ClockedValuePlan)> {
    let mut ordered: Vec<_> = values.iter().collect();
    ordered.sort_unstable_by_key(|(instance, _)| instance.index());
    ordered
        .into_iter()
        .map(|(instance, plan)| (*instance, plan))
        .collect()
}

pub(super) fn lower_clocked_value_owners<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    clocked_values: &HashMap<InstanceId, ClockedValuePlan>,
    clocks: &LoweredClocks<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    for (name, variable) in &flat.variables {
        let Some(plan) = clocked_values.get(&variable.instance_id).copied() else {
            continue;
        };
        let clock = clocks.id(&plan.clock, plan.ownership_span)?;
        let ownership = dae::DaeProvenance::source(plan.ownership_span)?;
        let coordinate = coordinates.get(name).copied().ok_or_else(|| {
            dae::DaeConstructionError::InvalidVariableRole {
                name: name.clone(),
                span: plan.ownership_span,
            }
        })?;
        construction.clocks(|clocks| match coordinate {
            Coordinate::DiscreteReal(variable) if plan.sampled => {
                clocks.own_sampled_discrete_real(clock, variable, ownership)?;
                Ok(())
            }
            Coordinate::DiscreteReal(variable) => {
                clocks.own_discrete_real(clock, variable, ownership)?;
                Ok(())
            }
            Coordinate::DiscreteValue(variable) if plan.sampled => {
                clocks.own_sampled_discrete_value(clock, variable, ownership)?;
                Ok(())
            }
            Coordinate::DiscreteValue(variable) => {
                clocks.own_discrete_value(clock, variable, ownership)?;
                Ok(())
            }
            _ => Err(dae::DaeConstructionError::InvalidVariableRole {
                name: name.clone(),
                span: plan.ownership_span,
            }),
        })?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ClockLattice, ClockRational};

    fn plan() -> ClockedValuePlan {
        ClockedValuePlan {
            clock: ClockPlan::periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                Span::DUMMY,
            ),
            ownership_span: Span::DUMMY,
            sampled: false,
        }
    }

    #[test]
    fn clocked_value_allocation_order_uses_instance_identity() {
        let mut values = HashMap::new();
        values.insert(InstanceId::new(9), plan());
        values.insert(InstanceId::new(2), plan());
        values.insert(InstanceId::new(5), plan());

        let ids: Vec<_> = clocked_values_in_instance_order(&values)
            .into_iter()
            .map(|(instance, _)| instance.index())
            .collect();

        assert_eq!(ids, [2, 5, 9]);
    }
}
