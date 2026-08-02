use std::marker::PhantomData;

use rumoca_core::{ClockLattice, PeriodicClockSchedule};

use crate::model::{Storage, check_provenance, checked_u32, unknown};
use crate::{
    ClockId, ClockOwnershipId, ConditionId, DaeConstructionError, DaeProvenance, DiscreteRealId,
    DiscreteValueId, PeriodicClockId, VariableId, VariableRole,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ClockKind {
    Periodic(PeriodicClockSchedule),
    Triggered(u32),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ClockEntry {
    pub(crate) kind: ClockKind,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ClockOwnershipEntry {
    pub(crate) variable: u32,
    pub(crate) clock: u32,
    pub(crate) sampled: bool,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy)]
pub enum ClockOperation<'dae> {
    Periodic(&'dae PeriodicClockSchedule),
    Triggered(ConditionId<'dae>),
}

#[derive(Debug, Clone, Copy)]
pub struct ClockView<'dae> {
    pub(crate) operation: ClockOperation<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> ClockView<'dae> {
    pub const fn operation(self) -> ClockOperation<'dae> {
        self.operation
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClockedVariableKind {
    DiscreteReal,
    DiscreteValue,
}

#[derive(Debug, Clone, Copy)]
pub struct ClockOwnershipView<'dae> {
    pub(crate) variable: VariableId<'dae>,
    pub(crate) kind: ClockedVariableKind,
    pub(crate) clock: ClockId<'dae>,
    pub(crate) sampled: bool,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> ClockOwnershipView<'dae> {
    pub const fn variable(self) -> VariableId<'dae> {
        self.variable
    }

    pub const fn kind(self) -> ClockedVariableKind {
        self.kind
    }

    pub const fn clock(self) -> ClockId<'dae> {
        self.clock
    }

    /// Whether this owner is the coordinate defined by MLS §16.5.1
    /// `sample(u)`, whose source is read at the clock tick's left limit.
    pub const fn sampled(self) -> bool {
        self.sampled
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

pub struct Clocks<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Clocks<'_, 'dae> {
    pub fn periodic(
        &mut self,
        lattice: ClockLattice,
        provenance: DaeProvenance,
    ) -> Result<PeriodicClockId<'dae>, DaeConstructionError> {
        let schedule = PeriodicClockSchedule::absolute(lattice).map_err(|source| {
            DaeConstructionError::InvalidClockLattice {
                source,
                span: provenance.span(),
            }
        })?;
        self.scheduled(schedule, provenance)
    }

    /// Construct a periodic clock whose exact phase is relative to the
    /// simulation start instant.
    pub fn periodic_from_simulation_start(
        &mut self,
        lattice: ClockLattice,
        provenance: DaeProvenance,
    ) -> Result<PeriodicClockId<'dae>, DaeConstructionError> {
        let schedule =
            PeriodicClockSchedule::simulation_start_relative(lattice).map_err(|source| {
                DaeConstructionError::InvalidClockLattice {
                    source,
                    span: provenance.span(),
                }
            })?;
        self.scheduled(schedule, provenance)
    }

    pub fn scheduled(
        &mut self,
        schedule: PeriodicClockSchedule,
        provenance: DaeProvenance,
    ) -> Result<PeriodicClockId<'dae>, DaeConstructionError> {
        let schedule = match schedule.anchor() {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                PeriodicClockSchedule::absolute(schedule.lattice())
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                PeriodicClockSchedule::simulation_start_relative(schedule.lattice())
            }
        }
        .map_err(|source| DaeConstructionError::InvalidClockLattice {
            source,
            span: provenance.span(),
        })?;
        self.insert(ClockKind::Periodic(schedule), provenance)
            .map(|clock| PeriodicClockId::from_raw(clock.index()))
    }

    pub fn triggered(
        &mut self,
        condition: ConditionId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ClockId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .conditions
            .get(condition.index() as usize)
            .ok_or_else(|| unknown("condition", condition.index(), provenance))?;
        self.insert(ClockKind::Triggered(condition.index()), provenance)
    }

    pub fn own_discrete_real(
        &mut self,
        clock: ClockId<'dae>,
        variable: DiscreteRealId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ClockOwnershipId<'dae>, DaeConstructionError> {
        self.own(
            clock,
            variable.index(),
            VariableRole::DiscreteReal,
            false,
            provenance,
        )
    }

    pub fn own_discrete_value(
        &mut self,
        clock: ClockId<'dae>,
        variable: DiscreteValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ClockOwnershipId<'dae>, DaeConstructionError> {
        self.own(
            clock,
            variable.index(),
            VariableRole::DiscreteValue,
            false,
            provenance,
        )
    }

    /// Own a discrete Real coordinate defined by MLS §16.5.1 `sample(u)`.
    pub fn own_sampled_discrete_real(
        &mut self,
        clock: ClockId<'dae>,
        variable: DiscreteRealId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ClockOwnershipId<'dae>, DaeConstructionError> {
        self.own(
            clock,
            variable.index(),
            VariableRole::DiscreteReal,
            true,
            provenance,
        )
    }

    /// Own a discrete-valued coordinate defined by MLS §16.5.1 `sample(u)`.
    pub fn own_sampled_discrete_value(
        &mut self,
        clock: ClockId<'dae>,
        variable: DiscreteValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ClockOwnershipId<'dae>, DaeConstructionError> {
        self.own(
            clock,
            variable.index(),
            VariableRole::DiscreteValue,
            true,
            provenance,
        )
    }

    fn insert(
        &mut self,
        kind: ClockKind,
        provenance: DaeProvenance,
    ) -> Result<ClockId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let raw = checked_u32(self.storage.clocks.len(), "clock arena", provenance)?;
        self.storage.clocks.push(ClockEntry { kind, provenance });
        Ok(ClockId::from_raw(raw))
    }

    fn own(
        &mut self,
        clock: ClockId<'dae>,
        variable: u32,
        expected_role: VariableRole,
        sampled: bool,
        provenance: DaeProvenance,
    ) -> Result<ClockOwnershipId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .clocks
            .get(clock.index() as usize)
            .ok_or_else(|| unknown("clock", clock.index(), provenance))?;
        let variable_entry = self.storage.variable(variable, provenance)?;
        if variable_entry.role != expected_role {
            return Err(DaeConstructionError::InvalidVariableRole {
                name: variable_entry.name.clone(),
                span: provenance.span(),
            });
        }
        if let Some(&index) = self.storage.clock_ownership_by_variable.get(&variable) {
            let entry = self
                .storage
                .clock_ownerships
                .get(index as usize)
                .expect("clock ownership index points into its dense arena");
            if entry.clock == clock.index() && entry.sampled == sampled {
                return Ok(ClockOwnershipId::from_raw(index));
            }
            return Err(DaeConstructionError::ConflictingClockOwnership {
                variable,
                established_clock: entry.clock,
                attempted_clock: clock.index(),
                established: entry.provenance,
                attempted: provenance,
            });
        }
        let raw = checked_u32(
            self.storage.clock_ownerships.len(),
            "clock ownership arena",
            provenance,
        )?;
        self.storage.clock_ownerships.push(ClockOwnershipEntry {
            variable,
            clock: clock.index(),
            sampled,
            provenance,
        });
        let prior = self
            .storage
            .clock_ownership_by_variable
            .insert(variable, raw);
        debug_assert!(prior.is_none());
        Ok(ClockOwnershipId::from_raw(raw))
    }
}
