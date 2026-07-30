use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::LowerError;

pub(super) struct LoweredClocks<'dae> {
    pub(super) partition: solve::SolveClockPartition,
    dae_clocks: Vec<solve::PeriodicClockId>,
    variable_owners: Vec<Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>>,
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
}

pub(super) fn lower_clocks<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<LoweredClocks<'dae>, LowerError> {
    let mut partition = solve::SolveClockPartition::default();
    let mut dae_clocks = Vec::with_capacity(view.clock_count());
    for index in 0..view.clock_count() {
        let dae_clock = view
            .clock_id(index)
            .expect("dense checked clock identity resolves");
        let clock = view
            .clock(dae_clock)
            .expect("checked clock identity resolves");
        let dae::ClockOperation::Periodic(lattice) = clock.operation() else {
            return Err(LowerError::unsupported(
                "triggered clocks do not yet have checked Solve scheduling",
                clock.provenance().span(),
            ));
        };
        let schedule = solve::PeriodicEventSchedule::new(*lattice).map_err(|error| {
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
    }

    Ok(LoweredClocks {
        partition,
        dae_clocks,
        variable_owners,
        marker: std::marker::PhantomData,
    })
}
