//! Independent refinement check for the DAE-to-Solve variable catalog.
//!
//! The lowering producer is intentionally not reused here. This checker joins
//! variables only by their source occurrence and independently derives the
//! semantic facts, effective fixity, state-initialization class, variability,
//! tunability, and storage association that Solve construction must retain.
//! Names, source spans, and scalar labels are never correlation authorities.

use rumoca_core::{Fixity, SourceOccurrenceId};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

/// Opaque evidence that one live Solve root preserves its co-retained DAE
/// variable catalog.
///
/// Only this module can construct the receipt. It deliberately implements
/// neither `Clone`, `Default`, nor serialization: its meaning depends on the
/// private live pairing retained by `LoweredSolveModel`.
pub(crate) struct CheckedDaeSolveVariableCatalogRefinement {
    _private: (),
}

/// Untrusted producer witness relating a DAE occurrence to the Solve catalog
/// ordinal issued for it.
///
/// The checker consumes and verifies this map. Construction performs no
/// validation and therefore grants no authority to the producer.
pub(crate) struct VariableCatalogTransferMap {
    pairs: Vec<VariableCatalogTransfer>,
}

struct VariableCatalogTransfer {
    occurrence: SourceOccurrenceId,
    solve_ordinal: usize,
}

impl VariableCatalogTransferMap {
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            pairs: Vec::with_capacity(capacity),
        }
    }

    pub(crate) fn record(&mut self, occurrence: SourceOccurrenceId, solve_ordinal: usize) {
        self.pairs.push(VariableCatalogTransfer {
            occurrence,
            solve_ordinal,
        });
    }
}

/// A failed DAE-to-Solve variable-catalog proof obligation.
#[derive(Debug, thiserror::Error)]
pub enum VariableCatalogRefinementError {
    #[error(
        "DAE-to-Solve variable transfer has {dae} DAE variables, {mapping} map entries, and {solve} Solve entries"
    )]
    Cardinality {
        dae: usize,
        mapping: usize,
        solve: usize,
    },
    #[error("DAE ordinal {dae_ordinal} maps to absent Solve ordinal {solve_ordinal}")]
    MissingMappedSolveVariable {
        dae_ordinal: usize,
        solve_ordinal: usize,
    },
    #[error("the producer transfer map has no entry at ordinal {ordinal}")]
    MissingMapping { ordinal: usize },
    #[error(
        "DAE ordinal {dae_ordinal} has occurrence {dae_occurrence:?}, but its transfer-map entry carries {mapped_occurrence:?}"
    )]
    DaeMapOccurrence {
        dae_ordinal: usize,
        dae_occurrence: SourceOccurrenceId,
        mapped_occurrence: SourceOccurrenceId,
    },
    #[error(
        "DAE ordinal {dae_ordinal} maps to Solve ordinal {solve_ordinal}; dense declaration order must be preserved"
    )]
    Reordered {
        dae_ordinal: usize,
        solve_ordinal: usize,
    },
    #[error(
        "transfer-map occurrence {mapped:?} does not equal Solve catalog occurrence {solve:?} at ordinal {ordinal}"
    )]
    MapSolveOccurrence {
        ordinal: usize,
        mapped: SourceOccurrenceId,
        solve: SourceOccurrenceId,
    },
    #[error("DAE source occurrence {occurrence:?} retains unsupported Record storage")]
    UnsupportedDaeScalarKind { occurrence: SourceOccurrenceId },
    #[error("source occurrence {occurrence:?} changed role from {dae_role:?} to {solve_role:?}")]
    Role {
        occurrence: SourceOccurrenceId,
        dae_role: dae::VariableRole,
        solve_role: solve::SolveVariableStorageRole,
    },
    #[error("source occurrence {occurrence:?} changed effective fixity from {dae:?} to {solve:?}")]
    Fixity {
        occurrence: SourceOccurrenceId,
        dae: Fixity,
        solve: Fixity,
    },
    #[error(
        "source occurrence {occurrence:?} with DAE role {dae_role:?} and fixity {dae_fixity:?} requires Solve state initialization {expected:?}, but found {actual:?}"
    )]
    StateInitialization {
        occurrence: SourceOccurrenceId,
        dae_role: dae::VariableRole,
        dae_fixity: Fixity,
        expected: solve::SolveStateInitialization,
        actual: solve::SolveStateInitialization,
    },
    #[error(
        "source occurrence {occurrence:?} with DAE variability {dae:?} and tunable={dae_tunable} requires Solve variability {expected:?}, but found {actual:?}"
    )]
    Variability {
        occurrence: SourceOccurrenceId,
        dae: dae::ExpressionVariability,
        dae_tunable: bool,
        expected: solve::SolveVariableVariability,
        actual: solve::SolveVariableVariability,
    },
    #[error(
        "source occurrence {occurrence:?} changed tunability from {dae} in DAE to {solve} in Solve"
    )]
    Tunability {
        occurrence: SourceOccurrenceId,
        dae: bool,
        solve: bool,
    },
    #[error(
        "source occurrence {occurrence:?} changed causality from {dae_causality:?} to {solve_causality:?}"
    )]
    Causality {
        occurrence: SourceOccurrenceId,
        dae_causality: dae::VariableCausality,
        solve_causality: solve::SolveVariableCausality,
    },
    #[error(
        "source occurrence {occurrence:?} changed scalar kind from {dae_kind:?} to {solve_kind:?}"
    )]
    ScalarKind {
        occurrence: SourceOccurrenceId,
        dae_kind: dae::ScalarType,
        solve_kind: solve::SolveVariableValueKind,
    },
    #[error("source occurrence {occurrence:?} changed dimensions from {dae:?} to {solve:?}")]
    Dimensions {
        occurrence: SourceOccurrenceId,
        dae: Vec<u32>,
        solve: Vec<u32>,
    },
    #[error("source occurrence {occurrence:?} changed scalar count from {dae} to {solve}")]
    ScalarCount {
        occurrence: SourceOccurrenceId,
        dae: usize,
        solve: usize,
    },
    #[error("storage arithmetic overflow for source occurrence {occurrence:?}")]
    StorageOverflow { occurrence: SourceOccurrenceId },
    #[error("no derived storage exists for source occurrence {occurrence:?}")]
    MissingDerivedStorage { occurrence: SourceOccurrenceId },
    #[error(
        "source occurrence {occurrence:?} has derived storage {expected:?}, but Solve declares {actual:?}"
    )]
    Storage {
        occurrence: SourceOccurrenceId,
        expected: solve::SolveVariableStorageRun,
        actual: solve::SolveVariableStorageRun,
    },
}

impl VariableCatalogRefinementError {
    /// Source occurrence whose DAE declaration best explains this refusal.
    ///
    /// This is diagnostic projection only. The checker has already made its
    /// decision without a span or presentation name.
    pub(crate) const fn source_occurrence(&self) -> Option<SourceOccurrenceId> {
        match self {
            Self::DaeMapOccurrence { dae_occurrence, .. } => Some(*dae_occurrence),
            Self::MapSolveOccurrence { mapped, .. } => Some(*mapped),
            Self::UnsupportedDaeScalarKind { occurrence }
            | Self::Role { occurrence, .. }
            | Self::Fixity { occurrence, .. }
            | Self::StateInitialization { occurrence, .. }
            | Self::Variability { occurrence, .. }
            | Self::Tunability { occurrence, .. }
            | Self::Causality { occurrence, .. }
            | Self::ScalarKind { occurrence, .. }
            | Self::Dimensions { occurrence, .. }
            | Self::ScalarCount { occurrence, .. }
            | Self::StorageOverflow { occurrence }
            | Self::MissingDerivedStorage { occurrence }
            | Self::Storage { occurrence, .. } => Some(*occurrence),
            Self::Cardinality { .. }
            | Self::MissingMappedSolveVariable { .. }
            | Self::MissingMapping { .. }
            | Self::Reordered { .. } => None,
        }
    }

    /// DAE ordinal whose declaration best explains a structural refusal.
    ///
    /// A cardinality error points at the first DAE entry absent from the map
    /// or Solve projection. Extra foreign entries intentionally have no DAE
    /// span.
    pub(crate) fn dae_ordinal(&self) -> Option<usize> {
        match self {
            Self::Cardinality {
                dae,
                mapping,
                solve,
            } if dae > mapping => Some(*mapping),
            Self::Cardinality { dae, solve, .. } if dae > solve => Some(*solve),
            Self::MissingMapping { ordinal } => Some(*ordinal),
            Self::MissingMappedSolveVariable { dae_ordinal, .. } => Some(*dae_ordinal),
            Self::DaeMapOccurrence { dae_ordinal, .. } | Self::Reordered { dae_ordinal, .. } => {
                Some(*dae_ordinal)
            }
            Self::MapSolveOccurrence { ordinal, .. } => Some(*ordinal),
            Self::Cardinality { .. }
            | Self::UnsupportedDaeScalarKind { .. }
            | Self::Role { .. }
            | Self::Fixity { .. }
            | Self::StateInitialization { .. }
            | Self::Variability { .. }
            | Self::Tunability { .. }
            | Self::Causality { .. }
            | Self::ScalarKind { .. }
            | Self::Dimensions { .. }
            | Self::ScalarCount { .. }
            | Self::StorageOverflow { .. }
            | Self::MissingDerivedStorage { .. }
            | Self::Storage { .. } => None,
        }
    }
}

struct DaeVariableFact {
    occurrence: SourceOccurrenceId,
    role: dae::VariableRole,
    fixed: Fixity,
    variability: dae::ExpressionVariability,
    is_tunable: bool,
    causality: dae::VariableCausality,
    scalar_type: dae::ScalarType,
    dimensions: Vec<u32>,
    scalar_count: usize,
}

struct SolveVariableFact {
    occurrence: SourceOccurrenceId,
    fixed: Fixity,
    state_initialization: solve::SolveStateInitialization,
    variability: solve::SolveVariableVariability,
    tunable: bool,
    causality: solve::SolveVariableCausality,
    dimensions: Vec<u32>,
    role: solve::SolveVariableStorageRole,
    value_kind: solve::SolveVariableValueKind,
    storage: solve::SolveVariableStorageRun,
}

pub(crate) fn check_variable_catalog_refinement(
    dae: dae::DaeVariableRefinementView,
    solve: solve::SolveVariableRefinementView,
    mapping: VariableCatalogTransferMap,
) -> Result<CheckedDaeSolveVariableCatalogRefinement, VariableCatalogRefinementError> {
    // Domain: `dae` and `solve` are unforgeable closed projections of
    // finalized roots with construction-issued unique occurrences. The mapping
    // is untrusted. Every loop below is bounded
    // by the checked common projection length and every size addition is
    // checked.
    //
    // Measured bounded L2 debt: materialization currently copies each compact
    // dimension slice twice per side (four copies per joined variable), and a
    // mismatch allocates the two diagnostic copies. Removing those copies must
    // preserve these closed inputs and the mutation-testable pure slice core.
    let dae_variables = project_dae_variables(dae);
    let solve_variables = project_solve_variables(solve);
    check_projected_variable_catalog_refinement(&dae_variables, &solve_variables, mapping)?;
    Ok(CheckedDaeSolveVariableCatalogRefinement { _private: () })
}

fn project_dae_variables(view: dae::DaeVariableRefinementView) -> Vec<DaeVariableFact> {
    let entries = view.entries();
    let mut variables = Vec::with_capacity(entries.len());
    let mut ordinal = 0;
    while ordinal < entries.len() {
        let variable = &entries[ordinal];
        variables.push(DaeVariableFact {
            occurrence: variable.source_occurrence(),
            role: variable.role(),
            fixed: variable.fixed(),
            variability: variable.variability(),
            is_tunable: variable.is_tunable(),
            causality: variable.causality(),
            scalar_type: variable.scalar_type(),
            dimensions: variable.dimensions().to_vec(),
            scalar_count: variable.scalar_count(),
        });
        ordinal += 1;
    }
    variables
}

fn project_solve_variables(view: solve::SolveVariableRefinementView) -> Vec<SolveVariableFact> {
    let entries = view.entries();
    let mut variables = Vec::with_capacity(entries.len());
    let mut ordinal = 0;
    while ordinal < entries.len() {
        let variable = &entries[ordinal];
        variables.push(SolveVariableFact {
            occurrence: variable.source_occurrence(),
            fixed: variable.fixed(),
            state_initialization: variable.state_initialization(),
            variability: variable.variability(),
            tunable: variable.tunable(),
            causality: variable.causality(),
            dimensions: variable.dimensions().to_vec(),
            role: variable.role(),
            value_kind: variable.value_kind(),
            storage: variable.storage(),
        });
        ordinal += 1;
    }
    variables
}

// Internal domain: facts from the two closed projections above. Raw slices are
// retained for fault injection, not as an alternate checked-root input route.
fn check_projected_variable_catalog_refinement(
    dae_variables: &[DaeVariableFact],
    solve_variables: &[SolveVariableFact],
    mapping: VariableCatalogTransferMap,
) -> Result<(), VariableCatalogRefinementError> {
    if dae_variables.len() != mapping.pairs.len() || dae_variables.len() != solve_variables.len() {
        return Err(VariableCatalogRefinementError::Cardinality {
            dae: dae_variables.len(),
            mapping: mapping.pairs.len(),
            solve: solve_variables.len(),
        });
    }

    let expected_storage = derive_storage(dae_variables)?;
    for (dae_ordinal, variable) in dae_variables.iter().enumerate() {
        let transfer = mapping.pairs.get(dae_ordinal).ok_or(
            VariableCatalogRefinementError::MissingMapping {
                ordinal: dae_ordinal,
            },
        )?;
        let occurrence = variable.occurrence;
        if transfer.occurrence != occurrence {
            return Err(VariableCatalogRefinementError::DaeMapOccurrence {
                dae_ordinal,
                dae_occurrence: occurrence,
                mapped_occurrence: transfer.occurrence,
            });
        }
        let entry = solve_variables.get(transfer.solve_ordinal).ok_or(
            VariableCatalogRefinementError::MissingMappedSolveVariable {
                dae_ordinal,
                solve_ordinal: transfer.solve_ordinal,
            },
        )?;
        if transfer.solve_ordinal != dae_ordinal {
            return Err(VariableCatalogRefinementError::Reordered {
                dae_ordinal,
                solve_ordinal: transfer.solve_ordinal,
            });
        }

        if entry.occurrence != transfer.occurrence {
            return Err(VariableCatalogRefinementError::MapSolveOccurrence {
                ordinal: transfer.solve_ordinal,
                mapped: transfer.occurrence,
                solve: entry.occurrence,
            });
        }

        let storage = expected_storage
            .get(dae_ordinal)
            .copied()
            .flatten()
            .ok_or(VariableCatalogRefinementError::MissingDerivedStorage { occurrence })?;
        check_variable(occurrence, variable, entry, storage)?;
    }

    Ok(())
}

fn check_variable(
    occurrence: SourceOccurrenceId,
    variable: &DaeVariableFact,
    entry: &SolveVariableFact,
    expected_storage: solve::SolveVariableStorageRun,
) -> Result<(), VariableCatalogRefinementError> {
    let expected_role = expected_role(variable);
    if entry.role != expected_role {
        return Err(VariableCatalogRefinementError::Role {
            occurrence,
            dae_role: variable.role,
            solve_role: entry.role,
        });
    }

    if entry.fixed != variable.fixed {
        return Err(VariableCatalogRefinementError::Fixity {
            occurrence,
            dae: variable.fixed,
            solve: entry.fixed,
        });
    }

    let expected_initialization = expected_state_initialization(variable);
    if entry.state_initialization != expected_initialization {
        return Err(VariableCatalogRefinementError::StateInitialization {
            occurrence,
            dae_role: variable.role,
            dae_fixity: variable.fixed,
            expected: expected_initialization,
            actual: entry.state_initialization,
        });
    }

    let expected_variability = expected_variability(variable);
    if entry.variability != expected_variability {
        return Err(VariableCatalogRefinementError::Variability {
            occurrence,
            dae: variable.variability,
            dae_tunable: variable.is_tunable,
            expected: expected_variability,
            actual: entry.variability,
        });
    }

    if entry.tunable != variable.is_tunable {
        return Err(VariableCatalogRefinementError::Tunability {
            occurrence,
            dae: variable.is_tunable,
            solve: entry.tunable,
        });
    }

    let expected_causality = expected_causality(variable.causality);
    if entry.causality != expected_causality {
        return Err(VariableCatalogRefinementError::Causality {
            occurrence,
            dae_causality: variable.causality,
            solve_causality: entry.causality,
        });
    }

    let dae_kind = variable.scalar_type;
    let expected_kind = expected_kind(occurrence, dae_kind)?;
    if entry.value_kind != expected_kind {
        return Err(VariableCatalogRefinementError::ScalarKind {
            occurrence,
            dae_kind,
            solve_kind: entry.value_kind,
        });
    }

    check_dimensions(occurrence, variable, entry)?;

    let dae_scalar_count = variable.scalar_count;
    if entry.storage.scalar_count != dae_scalar_count {
        return Err(VariableCatalogRefinementError::ScalarCount {
            occurrence,
            dae: dae_scalar_count,
            solve: entry.storage.scalar_count,
        });
    }
    if entry.storage != expected_storage {
        return Err(VariableCatalogRefinementError::Storage {
            occurrence,
            expected: expected_storage,
            actual: entry.storage,
        });
    }

    Ok(())
}

fn check_dimensions(
    occurrence: SourceOccurrenceId,
    variable: &DaeVariableFact,
    entry: &SolveVariableFact,
) -> Result<(), VariableCatalogRefinementError> {
    if entry.dimensions != variable.dimensions {
        return Err(VariableCatalogRefinementError::Dimensions {
            occurrence,
            dae: variable.dimensions.clone(),
            solve: entry.dimensions.clone(),
        });
    }
    Ok(())
}

const fn expected_state_initialization(
    variable: &DaeVariableFact,
) -> solve::SolveStateInitialization {
    match (variable.role, variable.fixed) {
        (dae::VariableRole::State, Fixity::Fixed) => solve::SolveStateInitialization::Exact,
        (dae::VariableRole::State, Fixity::Free) => solve::SolveStateInitialization::Approximate,
        (_, _) => solve::SolveStateInitialization::NotState,
    }
}

const fn expected_variability(variable: &DaeVariableFact) -> solve::SolveVariableVariability {
    match (variable.variability, variable.is_tunable) {
        (dae::ExpressionVariability::Constant, _) => solve::SolveVariableVariability::Constant,
        (dae::ExpressionVariability::Parameter, true) => solve::SolveVariableVariability::Tunable,
        (dae::ExpressionVariability::Parameter, false) => solve::SolveVariableVariability::Fixed,
        (dae::ExpressionVariability::Discrete, _) => solve::SolveVariableVariability::Discrete,
        (dae::ExpressionVariability::Continuous, _) => solve::SolveVariableVariability::Continuous,
    }
}

fn derive_storage(
    variables: &[DaeVariableFact],
) -> Result<Vec<Option<solve::SolveVariableStorageRun>>, VariableCatalogRefinementError> {
    let mut storage = Vec::with_capacity(variables.len());
    while storage.len() < variables.len() {
        storage.push(None);
    }
    derive_storage_column(
        variables,
        &mut storage,
        &[
            dae::VariableRole::State,
            dae::VariableRole::Algebraic,
            dae::VariableRole::Output,
        ],
        solve::SolveStorageColumn::Y,
    )?;
    derive_storage_column(
        variables,
        &mut storage,
        &[
            dae::VariableRole::Parameter,
            dae::VariableRole::Constant,
            dae::VariableRole::Input,
            dae::VariableRole::DiscreteReal,
            dae::VariableRole::DiscreteValue,
        ],
        solve::SolveStorageColumn::P,
    )?;
    Ok(storage)
}

fn derive_storage_column(
    variables: &[DaeVariableFact],
    storage: &mut [Option<solve::SolveVariableStorageRun>],
    roles: &[dae::VariableRole],
    column: solve::SolveStorageColumn,
) -> Result<(), VariableCatalogRefinementError> {
    let mut base = 0usize;
    let mut ordinal = 0;
    while ordinal < roles.len() {
        base = assign_storage_role(variables, storage, roles[ordinal], column, base)?;
        ordinal += 1;
    }
    Ok(())
}

fn assign_storage_role(
    variables: &[DaeVariableFact],
    storage: &mut [Option<solve::SolveVariableStorageRun>],
    role: dae::VariableRole,
    column: solve::SolveStorageColumn,
    mut base: usize,
) -> Result<usize, VariableCatalogRefinementError> {
    let mut ordinal = 0;
    while ordinal < variables.len() {
        let variable = &variables[ordinal];
        if variable.role == role {
            let occurrence = variable.occurrence;
            let scalar_count = variable.scalar_count;
            let value_kind = expected_kind(occurrence, variable.scalar_type)?;
            storage[ordinal] = Some(solve::SolveVariableStorageRun {
                base: solve::SolveStorageCoordinate::new(column, base),
                scalar_count,
                role: expected_role(variable),
                value_kind,
            });
            base = base
                .checked_add(scalar_count)
                .ok_or(VariableCatalogRefinementError::StorageOverflow { occurrence })?;
        }
        ordinal += 1;
    }
    Ok(base)
}

fn expected_role(variable: &DaeVariableFact) -> solve::SolveVariableStorageRole {
    if variable.causality == dae::VariableCausality::Input {
        return solve::SolveVariableStorageRole::ExternalInput;
    }
    match variable.role {
        dae::VariableRole::Parameter => solve::SolveVariableStorageRole::Parameter,
        dae::VariableRole::Constant => solve::SolveVariableStorageRole::Constant,
        dae::VariableRole::Input => solve::SolveVariableStorageRole::ExternalInput,
        dae::VariableRole::State => solve::SolveVariableStorageRole::State,
        dae::VariableRole::Algebraic => solve::SolveVariableStorageRole::Algebraic,
        dae::VariableRole::Output => solve::SolveVariableStorageRole::Output,
        dae::VariableRole::DiscreteReal => solve::SolveVariableStorageRole::DiscreteReal,
        dae::VariableRole::DiscreteValue => solve::SolveVariableStorageRole::DiscreteValue,
    }
}

const fn expected_causality(causality: dae::VariableCausality) -> solve::SolveVariableCausality {
    match causality {
        dae::VariableCausality::Input => solve::SolveVariableCausality::Input,
        dae::VariableCausality::Output => solve::SolveVariableCausality::Output,
        dae::VariableCausality::Parameter => solve::SolveVariableCausality::Parameter,
        dae::VariableCausality::CalculatedParameter => {
            solve::SolveVariableCausality::CalculatedParameter
        }
        dae::VariableCausality::Independent => solve::SolveVariableCausality::Independent,
        dae::VariableCausality::Local => solve::SolveVariableCausality::Local,
    }
}

fn expected_kind(
    occurrence: SourceOccurrenceId,
    kind: dae::ScalarType,
) -> Result<solve::SolveVariableValueKind, VariableCatalogRefinementError> {
    match kind {
        dae::ScalarType::Real => Ok(solve::SolveVariableValueKind::Real),
        dae::ScalarType::Integer => Ok(solve::SolveVariableValueKind::Integer),
        dae::ScalarType::Boolean => Ok(solve::SolveVariableValueKind::Boolean),
        dae::ScalarType::Enumeration => Ok(solve::SolveVariableValueKind::Enumeration),
        dae::ScalarType::String => Ok(solve::SolveVariableValueKind::String),
        dae::ScalarType::Record => {
            Err(VariableCatalogRefinementError::UnsupportedDaeScalarKind { occurrence })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rumoca_core::{InstanceId, SourceMap, Span, StateSelect, TypeId, VarName};

    use super::*;

    const PARAMETER_OCCURRENCE: u32 = 41;
    const CONSTANT_OCCURRENCE: u32 = 42;

    #[test]
    fn classification_preserves_primitive_kinds_and_refuses_record() {
        let source = occurrence(PARAMETER_OCCURRENCE);
        for (dae_kind, solve_kind) in [
            (dae::ScalarType::Real, solve::SolveVariableValueKind::Real),
            (
                dae::ScalarType::Integer,
                solve::SolveVariableValueKind::Integer,
            ),
            (
                dae::ScalarType::Enumeration,
                solve::SolveVariableValueKind::Enumeration,
            ),
            (
                dae::ScalarType::Boolean,
                solve::SolveVariableValueKind::Boolean,
            ),
            (
                dae::ScalarType::String,
                solve::SolveVariableValueKind::String,
            ),
        ] {
            assert_eq!(
                expected_kind(source, dae_kind).expect("primitive kinds have Solve counterparts"),
                solve_kind
            );
        }
        assert!(matches!(
            expected_kind(source, dae::ScalarType::Record),
            Err(VariableCatalogRefinementError::UnsupportedDaeScalarKind { occurrence })
                if occurrence == source
        ));
    }

    #[test]
    fn classification_preserves_every_causality_category() {
        for (source, target) in [
            (
                dae::VariableCausality::Input,
                solve::SolveVariableCausality::Input,
            ),
            (
                dae::VariableCausality::Output,
                solve::SolveVariableCausality::Output,
            ),
            (
                dae::VariableCausality::Parameter,
                solve::SolveVariableCausality::Parameter,
            ),
            (
                dae::VariableCausality::CalculatedParameter,
                solve::SolveVariableCausality::CalculatedParameter,
            ),
            (
                dae::VariableCausality::Independent,
                solve::SolveVariableCausality::Independent,
            ),
            (
                dae::VariableCausality::Local,
                solve::SolveVariableCausality::Local,
            ),
        ] {
            assert_eq!(expected_causality(source), target);
        }
    }

    #[test]
    fn classification_initialization_depends_on_state_role_and_fixity() {
        let mut fact = dae_fact();
        fact.role = dae::VariableRole::State;
        fact.fixed = Fixity::Fixed;
        assert_eq!(
            expected_state_initialization(&fact),
            solve::SolveStateInitialization::Exact
        );
        fact.fixed = Fixity::Free;
        assert_eq!(
            expected_state_initialization(&fact),
            solve::SolveStateInitialization::Approximate
        );
        fact.role = dae::VariableRole::Parameter;
        assert_eq!(
            expected_state_initialization(&fact),
            solve::SolveStateInitialization::NotState
        );
    }

    #[test]
    fn classification_variability_uses_tunability_only_for_parameters() {
        let mut fact = dae_fact();
        fact.variability = dae::ExpressionVariability::Parameter;
        fact.is_tunable = true;
        assert_eq!(
            expected_variability(&fact),
            solve::SolveVariableVariability::Tunable
        );
        fact.is_tunable = false;
        assert_eq!(
            expected_variability(&fact),
            solve::SolveVariableVariability::Fixed
        );
        for (source, target) in [
            (
                dae::ExpressionVariability::Constant,
                solve::SolveVariableVariability::Constant,
            ),
            (
                dae::ExpressionVariability::Discrete,
                solve::SolveVariableVariability::Discrete,
            ),
            (
                dae::ExpressionVariability::Continuous,
                solve::SolveVariableVariability::Continuous,
            ),
        ] {
            fact.variability = source;
            for tunable in [false, true] {
                fact.is_tunable = tunable;
                assert_eq!(expected_variability(&fact), target);
            }
        }
    }

    #[test]
    fn dimension_check_preserves_rank_order_and_refusal_payload() {
        let mut variable = dae_fact();
        let mut entry = SolveVariableFact {
            occurrence: variable.occurrence,
            fixed: Fixity::Fixed,
            state_initialization: solve::SolveStateInitialization::NotState,
            variability: solve::SolveVariableVariability::Fixed,
            tunable: false,
            causality: solve::SolveVariableCausality::Parameter,
            dimensions: Vec::new(),
            role: solve::SolveVariableStorageRole::Parameter,
            value_kind: solve::SolveVariableValueKind::Real,
            storage: solve::SolveVariableStorageRun {
                base: solve::SolveStorageCoordinate::P(0),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::Parameter,
                value_kind: solve::SolveVariableValueKind::Real,
            },
        };
        let cases: &[(&[u32], &[u32], bool)] = &[
            (&[], &[], true),
            (&[0], &[0], true),
            (&[2, 3], &[2, 3], true),
            (&[2, 3], &[3, 2], false),
            (&[2], &[2, 1], false),
            (&[2, 1], &[2], false),
            (&[0, 7], &[7, 0], false),
            (&[2, 3], &[2, 4], false),
        ];
        for &(dae_dimensions, solve_dimensions, should_accept) in cases {
            variable.dimensions = dae_dimensions.to_vec();
            entry.dimensions = solve_dimensions.to_vec();
            let result = check_dimensions(variable.occurrence, &variable, &entry);
            match (result, should_accept) {
                (Ok(()), true) => {}
                (
                    Err(VariableCatalogRefinementError::Dimensions {
                        occurrence,
                        dae,
                        solve,
                    }),
                    false,
                ) => {
                    assert_eq!(occurrence, variable.occurrence);
                    assert_eq!(dae, dae_dimensions);
                    assert_eq!(solve, solve_dimensions);
                }
                (actual, expected) => panic!("expected acceptance {expected}, got {actual:?}"),
            }
        }
    }

    fn dae_fact() -> DaeVariableFact {
        let dae = two_variable_real_dae(SecondVariableRole::Constant);
        dae.inspect(|view| {
            let mut facts = project_dae_variables(view.variable_refinement());
            facts.remove(0)
        })
    }

    #[test]
    fn two_variable_real_roots_issue_a_valid_refinement_receipt() {
        let (dae, solve) = two_variable_real_roots();
        let receipt = dae.inspect(|view| {
            check_variable_catalog_refinement(
                view.variable_refinement(),
                solve.variable_refinement(),
                valid_mapping(),
            )
        });
        assert!(receipt.is_ok());
        let facts = project_solve_variables(solve.variable_refinement());
        let catalog = solve.variable_catalog().entries();
        assert_eq!(catalog.len(), 2);
        assert_eq!(facts.len(), catalog.len());
        for (ordinal, fact) in facts.iter().enumerate() {
            let source = &catalog[ordinal];
            assert_eq!(fact.occurrence, source.source_occurrence());
            assert_eq!(fact.fixed, source.fixed());
            assert_eq!(fact.state_initialization, source.state_initialization());
            assert_eq!(fact.variability, source.variability());
            assert_eq!(fact.tunable, source.is_tunable());
            assert_eq!(fact.causality, source.causality());
            assert_eq!(fact.dimensions, source.dimensions());
            assert_eq!(fact.role, source.role());
            assert_eq!(fact.value_kind, source.value_kind());
            assert_eq!(fact.storage, source.storage());
        }
    }

    #[test]
    fn dae_fact_projection_preserves_nine_fields_in_order() {
        let dae = two_variable_real_dae(SecondVariableRole::Constant);
        dae.inspect(|view| {
            let facts = project_dae_variables(view.variable_refinement());
            assert_eq!(facts.len(), 2);
            assert_eq!(facts.len(), view.variable_count());
            for (ordinal, (_, source)) in view.variables().enumerate() {
                let fact = &facts[ordinal];
                assert_eq!(fact.occurrence, source.source_occurrence());
                assert_eq!(fact.role, source.role());
                assert_eq!(fact.fixed, source.fixed());
                assert_eq!(fact.variability, source.variability());
                assert_eq!(fact.is_tunable, source.is_tunable());
                assert_eq!(fact.causality, source.causality());
                assert_eq!(fact.scalar_type, source.value_type().scalar_type());
                assert_eq!(fact.dimensions, source.value_type().dimensions());
                assert_eq!(fact.scalar_count, source.scalar_count());
            }
        });
    }

    #[test]
    fn live_root_checker_refuses_a_foreign_solve_semantics() {
        let expected_dae = two_variable_real_dae(SecondVariableRole::Constant);
        let foreign_dae = two_variable_real_dae(SecondVariableRole::Parameter);
        let foreign_solve = lower_fixture(&foreign_dae);
        let rejected = expected_dae.inspect(|view| {
            check_variable_catalog_refinement(
                view.variable_refinement(),
                foreign_solve.variable_refinement(),
                valid_mapping(),
            )
        });
        assert!(matches!(
            rejected,
            Err(VariableCatalogRefinementError::Role {
                occurrence: source_occurrence,
                ..
            }) if source_occurrence == occurrence(CONSTANT_OCCURRENCE)
        ));
    }

    #[test]
    fn refinement_refusal_projects_the_exact_dae_declaration_span_after_checking() {
        // The only direction exercised here is proof refusal -> diagnostic:
        // neither a span nor a presentation name can construct checker input.
        let dae = two_variable_real_dae(SecondVariableRole::Constant);
        let expected_span = dae.inspect(|view| {
            view.variables()
                .nth(1)
                .expect("the fixture has a second declaration")
                .1
                .declaration()
                .span()
        });
        let diagnostic = crate::model_values::variable_catalog_refinement_error(
            &dae,
            VariableCatalogRefinementError::Role {
                occurrence: occurrence(CONSTANT_OCCURRENCE),
                dae_role: dae::VariableRole::Constant,
                solve_role: solve::SolveVariableStorageRole::Parameter,
            },
        );
        assert_eq!(diagnostic.source_span(), Some(expected_span));

        let causality_diagnostic = crate::model_values::variable_catalog_refinement_error(
            &dae,
            VariableCatalogRefinementError::Causality {
                occurrence: occurrence(CONSTANT_OCCURRENCE),
                dae_causality: dae::VariableCausality::Local,
                solve_causality: solve::SolveVariableCausality::CalculatedParameter,
            },
        );
        assert_eq!(causality_diagnostic.source_span(), Some(expected_span));

        let missing_target = crate::model_values::variable_catalog_refinement_error(
            &dae,
            VariableCatalogRefinementError::MissingMappedSolveVariable {
                dae_ordinal: 1,
                solve_ordinal: 99,
            },
        );
        assert_eq!(missing_target.source_span(), Some(expected_span));
    }

    #[test]
    fn projected_checker_refuses_cardinality_mismatch() {
        let (dae, solve) = projected_fixture();
        let mut mapping = valid_mapping();
        mapping.pairs.pop();
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, mapping),
            Err(VariableCatalogRefinementError::Cardinality {
                dae: 2,
                mapping: 1,
                solve: 2,
            })
        ));
    }

    #[test]
    fn projected_checker_refuses_duplicate_mapped_occurrence() {
        let (dae, solve) = projected_fixture();
        let mut mapping = VariableCatalogTransferMap::new(2);
        mapping.record(occurrence(PARAMETER_OCCURRENCE), 0);
        mapping.record(occurrence(PARAMETER_OCCURRENCE), 1);
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, mapping),
            Err(VariableCatalogRefinementError::DaeMapOccurrence { dae_ordinal: 1, .. })
        ));
    }

    #[test]
    fn projected_checker_refuses_duplicate_mapped_ordinal() {
        let (dae, solve) = projected_fixture();
        let mut mapping = VariableCatalogTransferMap::new(2);
        mapping.record(occurrence(PARAMETER_OCCURRENCE), 0);
        mapping.record(occurrence(CONSTANT_OCCURRENCE), 0);
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, mapping),
            Err(VariableCatalogRefinementError::Reordered {
                dae_ordinal: 1,
                solve_ordinal: 0,
            })
        ));
    }

    #[test]
    fn projected_checker_refuses_dae_map_occurrence_mismatch() {
        let (dae, solve) = projected_fixture();
        let mut mapping = VariableCatalogTransferMap::new(2);
        mapping.record(occurrence(99), 0);
        mapping.record(occurrence(CONSTANT_OCCURRENCE), 1);
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, mapping),
            Err(VariableCatalogRefinementError::DaeMapOccurrence { dae_ordinal: 0, .. })
        ));
    }

    #[test]
    fn projected_checker_refuses_reordered_targets() {
        let (dae, solve) = projected_fixture();
        let mut mapping = VariableCatalogTransferMap::new(2);
        mapping.record(occurrence(PARAMETER_OCCURRENCE), 1);
        mapping.record(occurrence(CONSTANT_OCCURRENCE), 0);
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, mapping),
            Err(VariableCatalogRefinementError::Reordered {
                dae_ordinal: 0,
                solve_ordinal: 1,
            })
        ));
    }

    #[test]
    fn projected_checker_refuses_out_of_range_target() {
        let (dae, solve) = projected_fixture();
        let mut mapping = VariableCatalogTransferMap::new(2);
        mapping.record(occurrence(PARAMETER_OCCURRENCE), 2);
        mapping.record(occurrence(CONSTANT_OCCURRENCE), 1);
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, mapping),
            Err(VariableCatalogRefinementError::MissingMappedSolveVariable {
                dae_ordinal: 0,
                solve_ordinal: 2,
            })
        ));
    }

    #[test]
    fn projected_checker_refuses_map_solve_occurrence_mismatch() {
        let (dae, mut solve) = projected_fixture();
        solve[0].occurrence = occurrence(99);
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::MapSolveOccurrence { ordinal: 0, .. })
        ));
    }

    #[test]
    fn projected_checker_refuses_dae_occurrence_change_against_map() {
        let (mut dae, solve) = projected_fixture();
        dae[1].occurrence = dae[0].occurrence;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::DaeMapOccurrence { dae_ordinal: 1, .. })
        ));
    }

    #[test]
    fn projected_checker_refuses_solve_occurrence_change_against_map() {
        let (dae, mut solve) = projected_fixture();
        solve[1].occurrence = solve[0].occurrence;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::MapSolveOccurrence { ordinal: 1, .. })
        ));
    }

    #[test]
    fn projected_checker_refuses_role_change() {
        let (dae, mut solve) = projected_fixture();
        solve[0].role = solve::SolveVariableStorageRole::Constant;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Role { .. })
        ));
    }

    #[test]
    fn projected_checker_refuses_causality_change() {
        let (dae, mut solve) = projected_fixture();
        solve[0].causality = solve::SolveVariableCausality::CalculatedParameter;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Causality {
                dae_causality: dae::VariableCausality::Parameter,
                solve_causality: solve::SolveVariableCausality::CalculatedParameter,
                ..
            })
        ));
    }

    #[test]
    fn projected_checker_accepts_tunable_parameter_variability_relation() {
        let (mut dae, mut solve) = projected_fixture();
        dae[0].is_tunable = true;
        solve[0].variability = solve::SolveVariableVariability::Tunable;
        solve[0].tunable = true;
        assert!(check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()).is_ok());
    }

    #[test]
    fn projected_checker_refuses_nontunable_parameter_becoming_tunable() {
        let (dae, mut solve) = projected_fixture();
        solve[0].variability = solve::SolveVariableVariability::Tunable;
        solve[0].tunable = true;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Variability {
                dae: dae::ExpressionVariability::Parameter,
                dae_tunable: false,
                expected: solve::SolveVariableVariability::Fixed,
                actual: solve::SolveVariableVariability::Tunable,
                ..
            })
        ));
    }

    #[test]
    fn projected_checker_refuses_tunability_change() {
        let (dae, mut solve) = projected_fixture();
        solve[0].tunable = true;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Tunability {
                dae: false,
                solve: true,
                ..
            })
        ));
    }

    #[test]
    fn projected_checker_accepts_fixed_and_free_state_initialization_relations() {
        let (mut dae, mut solve) = projected_fixed_state_fixture();
        assert!(check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()).is_ok());

        dae[0].fixed = Fixity::Free;
        solve[0].fixed = Fixity::Free;
        solve[0].state_initialization = solve::SolveStateInitialization::Approximate;
        assert!(check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()).is_ok());
    }

    #[test]
    fn projected_checker_refuses_fixed_to_free_producer_mutation() {
        let (dae, mut solve) = projected_fixed_state_fixture();
        solve[0].fixed = Fixity::Free;
        solve[0].state_initialization = solve::SolveStateInitialization::Approximate;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Fixity {
                dae: Fixity::Fixed,
                solve: Fixity::Free,
                ..
            })
        ));
    }

    #[test]
    fn projected_checker_refuses_state_initialization_change() {
        let (dae, mut solve) = projected_fixed_state_fixture();
        solve[0].state_initialization = solve::SolveStateInitialization::Approximate;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::StateInitialization {
                dae_role: dae::VariableRole::State,
                dae_fixity: Fixity::Fixed,
                expected: solve::SolveStateInitialization::Exact,
                actual: solve::SolveStateInitialization::Approximate,
                ..
            })
        ));
    }

    #[test]
    fn projected_checker_refuses_transposed_fixity_and_state_initialization_pairs() {
        let (dae, mut solve) = projected_two_state_fixture();
        assert!(check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()).is_ok());

        let first_fixed = solve[0].fixed;
        let first_initialization = solve[0].state_initialization;
        solve[0].fixed = solve[1].fixed;
        solve[0].state_initialization = solve[1].state_initialization;
        solve[1].fixed = first_fixed;
        solve[1].state_initialization = first_initialization;

        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Fixity {
                occurrence: actual_occurrence,
                dae: Fixity::Fixed,
                solve: Fixity::Free,
            }) if actual_occurrence == occurrence(PARAMETER_OCCURRENCE)
        ));
    }

    #[test]
    fn projected_checker_refuses_scalar_kind_change() {
        let (dae, mut solve) = projected_fixture();
        solve[0].value_kind = solve::SolveVariableValueKind::Integer;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::ScalarKind { .. })
        ));
    }

    #[test]
    fn projected_checker_refuses_dimension_change_with_equal_scalar_count() {
        let (dae, mut solve) = projected_fixture();
        assert_eq!(dae[0].scalar_count, 1);
        assert_eq!(solve[0].storage.scalar_count, 1);
        solve[0].dimensions = vec![1];
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Dimensions { .. })
        ));
    }

    #[test]
    fn projected_checker_refuses_scalar_count_change() {
        let (dae, mut solve) = projected_fixture();
        solve[0].storage.scalar_count = 2;
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::ScalarCount {
                dae: 1,
                solve: 2,
                ..
            })
        ));
    }

    #[test]
    fn projected_checker_refuses_storage_base_change() {
        for base in [
            solve::SolveStorageCoordinate::P(1),
            solve::SolveStorageCoordinate::Y(0),
        ] {
            let (dae, mut solve) = projected_fixture();
            assert_eq!(solve[0].storage.base, solve::SolveStorageCoordinate::P(0));
            solve[0].storage.base = base;
            assert!(matches!(
                check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
                Err(VariableCatalogRefinementError::Storage { .. })
            ));
        }
    }

    #[test]
    fn projected_checker_refuses_storage_role_change() {
        let (dae, mut solve) = projected_fixture();
        let expected = solve[0].storage;
        assert!(check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()).is_ok());
        solve[0].storage.role = solve::SolveVariableStorageRole::Constant;
        assert_eq!(solve[0].role, solve::SolveVariableStorageRole::Parameter);
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Storage {
                expected: original,
                actual,
                ..
            }) if original == expected && actual == solve[0].storage
        ));
    }

    #[test]
    fn projected_checker_refuses_storage_kind_change() {
        let (dae, mut solve) = projected_fixture();
        let expected = solve[0].storage;
        assert!(check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()).is_ok());
        solve[0].storage.value_kind = solve::SolveVariableValueKind::Integer;
        assert_eq!(solve[0].value_kind, solve::SolveVariableValueKind::Real);
        assert!(matches!(
            check_projected_variable_catalog_refinement(&dae, &solve, valid_mapping()),
            Err(VariableCatalogRefinementError::Storage {
                expected: original,
                actual,
                ..
            }) if original == expected && actual == solve[0].storage
        ));
    }

    fn projected_fixture() -> (Vec<DaeVariableFact>, Vec<SolveVariableFact>) {
        let (dae, solve) = two_variable_real_roots();
        let dae_variables = dae.inspect(|view| project_dae_variables(view.variable_refinement()));
        let solve_variables = project_solve_variables(solve.variable_refinement());
        (dae_variables, solve_variables)
    }

    #[test]
    fn derived_storage_preserves_role_priority_order_and_independent_columns() {
        use dae::VariableRole as R;
        use solve::SolveStorageCoordinate::{P, Y};
        let cases = [
            (R::Output, 2, Y(25)),
            (R::DiscreteValue, 4, P(31)),
            (R::State, 3, Y(0)),
            (R::Constant, 0, P(7)),
            (R::Algebraic, 5, Y(20)),
            (R::Parameter, 7, P(0)),
            (R::DiscreteReal, 11, P(20)),
            (R::Input, 13, P(7)),
            (R::State, 17, Y(3)),
        ];
        let facts: Vec<_> = cases
            .iter()
            .enumerate()
            .map(|(ordinal, &(role, scalar_count, _))| {
                let (mut fixture, _) = projected_fixture();
                let mut fact = fixture.remove(0);
                fact.occurrence = occurrence(u32::try_from(ordinal + 1).unwrap());
                fact.role = role;
                fact.scalar_count = scalar_count;
                fact.causality = dae::VariableCausality::Local;
                fact
            })
            .collect();
        let actual = derive_storage(&facts).expect("all role prefixes fit");
        assert_eq!(actual.len(), cases.len());
        for (actual, &(_, count, base)) in actual.iter().zip(&cases) {
            let actual = actual.expect("every enum role receives a storage run");
            assert_eq!(actual.base, base);
            assert_eq!(actual.scalar_count, count);
            assert_eq!(actual.value_kind, solve::SolveVariableValueKind::Real);
        }
    }

    #[test]
    fn derived_storage_refuses_first_role_order_overflow_before_later_record() {
        let (mut facts, _) = projected_fixed_state_fixture();
        facts[0].scalar_count = usize::MAX;
        let (mut second_fixture, _) = projected_fixed_state_fixture();
        let mut second_state = second_fixture.remove(0);
        second_state.occurrence = occurrence(17);
        second_state.scalar_count = 1;
        facts[1].scalar_type = dae::ScalarType::Record;
        facts.push(second_state);
        assert!(matches!(
            derive_storage(&facts),
            Err(VariableCatalogRefinementError::StorageOverflow { occurrence: source })
                if source == occurrence(17)
        ));

        facts[2].scalar_count = 0;
        assert!(matches!(
            derive_storage(&facts),
            Err(VariableCatalogRefinementError::UnsupportedDaeScalarKind { occurrence: source })
                if source == facts[1].occurrence
        ));
        facts[1].scalar_type = dae::ScalarType::Real;
        let storage = derive_storage(&facts).expect("a full Y column does not consume P space");
        assert_eq!(
            storage[1].unwrap().base,
            solve::SolveStorageCoordinate::P(0)
        );
        assert_eq!(
            storage[2].unwrap().base,
            solve::SolveStorageCoordinate::Y(usize::MAX)
        );
    }

    fn projected_fixed_state_fixture() -> (Vec<DaeVariableFact>, Vec<SolveVariableFact>) {
        let (mut dae, mut solve) = projected_fixture();
        dae[0].role = dae::VariableRole::State;
        dae[0].variability = dae::ExpressionVariability::Continuous;
        dae[0].causality = dae::VariableCausality::Local;
        solve[0].causality = solve::SolveVariableCausality::Local;
        solve[0].variability = solve::SolveVariableVariability::Continuous;
        solve[0].role = solve::SolveVariableStorageRole::State;
        solve[0].storage = solve::SolveVariableStorageRun {
            base: solve::SolveStorageCoordinate::Y(0),
            scalar_count: 1,
            role: solve::SolveVariableStorageRole::State,
            value_kind: solve::SolveVariableValueKind::Real,
        };
        solve[0].state_initialization = solve::SolveStateInitialization::Exact;
        solve[1].storage = solve::SolveVariableStorageRun {
            base: solve::SolveStorageCoordinate::P(0),
            scalar_count: 1,
            role: solve::SolveVariableStorageRole::Constant,
            value_kind: solve::SolveVariableValueKind::Real,
        };
        (dae, solve)
    }

    fn projected_two_state_fixture() -> (Vec<DaeVariableFact>, Vec<SolveVariableFact>) {
        let (mut dae, mut solve) = projected_fixture();
        for ordinal in 0..2 {
            dae[ordinal].role = dae::VariableRole::State;
            dae[ordinal].variability = dae::ExpressionVariability::Continuous;
            dae[ordinal].causality = dae::VariableCausality::Local;
            solve[ordinal].causality = solve::SolveVariableCausality::Local;
            solve[ordinal].variability = solve::SolveVariableVariability::Continuous;
            solve[ordinal].role = solve::SolveVariableStorageRole::State;
            solve[ordinal].storage = solve::SolveVariableStorageRun {
                base: solve::SolveStorageCoordinate::Y(ordinal),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::State,
                value_kind: solve::SolveVariableValueKind::Real,
            };
        }
        dae[1].fixed = Fixity::Free;
        solve[0].state_initialization = solve::SolveStateInitialization::Exact;
        solve[1].fixed = Fixity::Free;
        solve[1].state_initialization = solve::SolveStateInitialization::Approximate;
        (dae, solve)
    }

    fn two_variable_real_roots() -> (dae::Dae, solve::SolveModel) {
        let dae = two_variable_real_dae(SecondVariableRole::Constant);
        let solve = lower_fixture(&dae);
        (dae, solve)
    }

    fn lower_fixture(dae: &dae::Dae) -> solve::SolveModel {
        crate::lower_solve_model(dae, &HashMap::new(), |_| {})
            .expect("two bound Real declarations lower to one correlated Solve root")
            .into_model()
    }

    #[derive(Clone, Copy)]
    enum SecondVariableRole {
        Constant,
        Parameter,
    }

    fn two_variable_real_dae(second_role: SecondVariableRole) -> dae::Dae {
        let mut source_map = SourceMap::new();
        let source = source_map.add(
            "variable_catalog_refinement.mo",
            "parameter Real gain = 2; constant Real bias = 3;",
        );
        let parameter_at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 23))
            .expect("fixture parameter has source provenance");
        let constant_at = dae::DaeProvenance::source(Span::from_offsets(source, 25, 48))
            .expect("fixture constant has source provenance");
        dae::Dae::construct(source_map, |model| {
            let real = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    parameter_at,
                )
            })?;
            let (gain, bias) = model.expressions(|expressions| {
                Ok((
                    expressions
                        .at(parameter_at)
                        .literal(dae::DaeLiteral::Real(2.0))?,
                    expressions
                        .at(constant_at)
                        .literal(dae::DaeLiteral::Real(3.0))?,
                ))
            })?;
            model.variables(|variables| {
                variables.parameter(
                    VarName::new("gain"),
                    InstanceId::new(PARAMETER_OCCURRENCE),
                    real,
                    parameter_at,
                    explicit_attributes(gain, dae::VariableCausality::Parameter, "gain"),
                )?;
                match second_role {
                    SecondVariableRole::Constant => variables.constant(
                        VarName::new("bias"),
                        InstanceId::new(CONSTANT_OCCURRENCE),
                        real,
                        constant_at,
                        explicit_attributes(bias, dae::VariableCausality::Local, "bias"),
                    ),
                    SecondVariableRole::Parameter => variables.parameter(
                        VarName::new("bias"),
                        InstanceId::new(CONSTANT_OCCURRENCE),
                        real,
                        constant_at,
                        explicit_attributes(bias, dae::VariableCausality::Parameter, "bias"),
                    ),
                }
                .map(|_| ())
            })
        })
        .expect("two distinct nonzero occurrences construct a finalized DAE")
    }

    fn explicit_attributes<'dae>(
        binding: dae::ExprId<'dae>,
        causality: dae::VariableCausality,
        description: &str,
    ) -> dae::VariableAttributes<'dae> {
        dae::VariableAttributes {
            component_ref: None,
            binding: Some(binding),
            start: None,
            fixed: Some(rumoca_core::Fixity::Fixed),
            min: None,
            max: None,
            nominal: None,
            unit: Some("1".to_owned()),
            state_select: StateSelect::Default,
            description: Some(description.to_owned()),
            causality,
            is_tunable: false,
            is_held: false,
            origin: dae::VariableOrigin::Source,
        }
    }

    fn valid_mapping() -> VariableCatalogTransferMap {
        let mut mapping = VariableCatalogTransferMap::new(2);
        mapping.record(occurrence(PARAMETER_OCCURRENCE), 0);
        mapping.record(occurrence(CONSTANT_OCCURRENCE), 1);
        mapping
    }

    fn occurrence(value: u32) -> SourceOccurrenceId {
        SourceOccurrenceId::try_from(InstanceId::new(value))
            .expect("fixture source occurrences are explicit and nonzero")
    }
}
