//! Whole-batch validation against the retained component's original inventory.

use rumoca_ir_solve::SolveVariableValueKind;
use rumoca_ir_solve::fmi::{
    FmiCausality, FmiMetadata, FmiStorageColumn, FmiVariable, reads_input_lanes,
};

use super::super::{MeError, contract};

/// What a typed or Float64 batch may do to the variables it names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Access {
    /// Read any published variable of an executing component.
    Read,
    /// Read the host-written cells (inputs, parameters, constants) of a
    /// component that has not executed.
    HostCells,
    /// Write continuous-time inputs.
    WriteInputs,
    /// Write inputs and settable parameters: the window before
    /// `ExitInitializationMode` (FMI 3.0.2 state table).
    WriteInputsAndParameters,
}

impl Access {
    fn admits(self, variable: &FmiVariable) -> bool {
        let causality = variable.causality();
        match self {
            Self::Read => true,
            Self::HostCells => reads_input_lanes(variable),
            Self::WriteInputs => causality == FmiCausality::Input,
            Self::WriteInputsAndParameters => {
                matches!(causality, FmiCausality::Input | FmiCausality::Parameter)
            }
        }
    }
}

pub(super) fn checked_request<'inventory>(
    metadata: &'inventory FmiMetadata,
    references: &[u32],
    value_count: usize,
    kind: SolveVariableValueKind,
    access: Access,
) -> Result<Vec<&'inventory FmiVariable>, MeError> {
    let mut variables = Vec::with_capacity(references.len());
    let mut expected = 0usize;
    for &reference in references {
        let variable = lookup(metadata, reference, kind, access)?;
        expected = expected
            .checked_add(
                variable
                    .storage()
                    .expect("original storage checked")
                    .scalar_count(),
            )
            .ok_or_else(|| {
                contract("typed FMI batch scalar cardinality exceeds addressable storage")
            })?;
        variables.push(variable);
    }
    if expected != value_count {
        return Err(contract(format!(
            "typed FMI batch requires {expected} values, got {value_count}"
        )));
    }
    Ok(variables)
}

fn lookup(
    metadata: &FmiMetadata,
    reference: u32,
    kind: SolveVariableValueKind,
    access: Access,
) -> Result<&FmiVariable, MeError> {
    let variable = reference
        .checked_sub(1)
        .and_then(|index| metadata.variables().get(index as usize))
        .filter(|variable| variable.value_reference_fmi3() == reference)
        .ok_or_else(|| contract(format!("value reference {reference} is undeclared")))?;
    if variable.value_kind() != kind {
        return Err(contract(format!(
            "value reference {reference} has a different FMI type"
        )));
    }
    let storage = variable.storage().ok_or_else(|| {
        contract(format!(
            "value reference {reference} has no native typed storage"
        ))
    })?;
    if kind != SolveVariableValueKind::Real && storage.column() != FmiStorageColumn::P {
        return Err(contract(format!(
            "value reference {reference} has no native typed P storage"
        )));
    }
    if !access.admits(variable)
        || (access != Access::Read && storage.column() != FmiStorageColumn::P)
    {
        return Err(contract(format!(
            "value reference {reference} is not admitted for {access:?}"
        )));
    }
    Ok(variable)
}

pub(super) enum ReadMember<'inventory> {
    Time,
    Variable(&'inventory FmiVariable),
}

impl<'inventory> ReadMember<'inventory> {
    pub(super) fn variable(&self) -> Option<&'inventory FmiVariable> {
        match self {
            Self::Time => None,
            Self::Variable(variable) => Some(*variable),
        }
    }

    pub(super) fn scalar_count(&self) -> usize {
        match self {
            Self::Time => 1,
            Self::Variable(variable) => variable
                .storage()
                .expect("original storage checked")
                .scalar_count(),
        }
    }
}

pub(super) fn read_request<'inventory>(
    metadata: &'inventory FmiMetadata,
    references: &[u32],
    kind: SolveVariableValueKind,
    pending: bool,
) -> Result<(Vec<ReadMember<'inventory>>, usize), MeError> {
    let mut members = Vec::with_capacity(references.len());
    let mut count = 0usize;
    for &reference in references {
        let member = if reference == rumoca_ir_solve::fmi::FMI3_TIME_VALUE_REFERENCE {
            if kind != SolveVariableValueKind::Real {
                return Err(contract("independent time is a Float64 variable"));
            }
            if pending {
                return Err(contract(
                    "prepared native resource has no actual initialization time",
                ));
            }
            ReadMember::Time
        } else {
            ReadMember::Variable(lookup(
                metadata,
                reference,
                kind,
                if pending {
                    Access::HostCells
                } else {
                    Access::Read
                },
            )?)
        };
        count = count.checked_add(member.scalar_count()).ok_or_else(|| {
            contract("typed FMI batch scalar cardinality exceeds addressable storage")
        })?;
        members.push(member);
    }
    Ok((members, count))
}
