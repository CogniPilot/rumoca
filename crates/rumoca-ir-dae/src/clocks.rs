use std::marker::PhantomData;

use rumoca_core::ClockLattice;

use crate::model::{Storage, check_provenance, checked_u32, unknown};
use crate::{
    ClockId, ClockOwnershipId, ConditionId, DaeConstructionError, DaeProvenance, DiscreteRealId,
    DiscreteValueId, VariableId, VariableRole,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ClockKind {
    Periodic(ClockLattice),
    Triggered(u32),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ClockEntry {
    pub(crate) kind: ClockKind,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ClockedVariableRole {
    DiscreteReal,
    DiscreteValue,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ClockOwnershipEntry {
    pub(crate) variable: u32,
    pub(crate) role: ClockedVariableRole,
    pub(crate) clock: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy)]
pub enum ClockOperation<'dae> {
    Periodic(&'dae ClockLattice),
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
    ) -> Result<ClockId<'dae>, DaeConstructionError> {
        let lattice = ClockLattice::new(lattice.period(), lattice.phase()).map_err(|source| {
            DaeConstructionError::InvalidClockLattice {
                source,
                span: provenance.span(),
            }
        })?;
        self.insert(ClockKind::Periodic(lattice), provenance)
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
            ClockedVariableRole::DiscreteReal,
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
            ClockedVariableRole::DiscreteValue,
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
        role: ClockedVariableRole,
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
        if let Some((index, entry)) = self
            .storage
            .clock_ownerships
            .iter()
            .enumerate()
            .find(|(_, entry)| entry.variable == variable)
        {
            if entry.clock == clock.index() && entry.role == role {
                return Ok(ClockOwnershipId::from_raw(index as u32));
            }
            return Err(DaeConstructionError::DuplicateKey {
                kind: "clocked variable owner",
                key: variable_entry.name.to_string(),
                span: provenance.span(),
            });
        }
        let raw = checked_u32(
            self.storage.clock_ownerships.len(),
            "clock ownership arena",
            provenance,
        )?;
        self.storage.clock_ownerships.push(ClockOwnershipEntry {
            variable,
            role,
            clock: clock.index(),
            provenance,
        });
        Ok(ClockOwnershipId::from_raw(raw))
    }
}
