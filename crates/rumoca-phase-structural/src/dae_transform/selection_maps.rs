//! Coordinate maps a reduced state selection issues with its finalized DAE.

use rumoca_ir_dae as dae;

/// One admissible reduced state-selection chart, in the finalized transformed
/// DAE's own variable-ordinal space.
///
/// The reduced state selection picks a Dependent/Independent split of a
/// definitional first-integral coordinate group. A conserved first integral has
/// no globally injective reduced chart, so the fixed primary split folds when a
/// dependent coordinate passes through zero. This records one alternate split of
/// that same group: the `dependent` coordinates are reconstructed and the
/// `independent` coordinates are integrated. Each coordinate is a
/// `(transformed variable ordinal, scalar)` pair naming a scalar of the finalized
/// DAE that `PreparedDae::inspect` binds. The primary split is chart index zero.
#[derive(Clone, Debug)]
pub struct PreparedReducedChart {
    pub dependent: Box<[(u32, u32)]>,
    pub independent: Box<[(u32, u32)]>,
    /// Reciprocal conditioning of this chart's dependent Jacobian at the
    /// construction trial point, and the singular threshold it is measured
    /// against. The mirror of a folding coordinate may sit at or below the
    /// threshold here because it is regular at a different configuration.
    pub trial_rcond: f64,
    pub trial_singular_threshold: f64,
}

/// The source scalar one integrated state coordinate equals: the `order`-th
/// time derivative of scalar `scalar` of declaration `variable`, a transformed
/// variable ordinal. Order zero is the declared value itself.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PreparedStateCoordinate {
    pub variable: u32,
    pub order: u32,
    pub scalar: u32,
}

impl PreparedStateCoordinate {
    /// The name of the source declaration scalar in `view` this coordinate
    /// equals at order zero and differentiates at a higher order.
    pub fn source_scalar_name(&self, view: dae::DaeView<'_>) -> Option<String> {
        let id = view.variable_id(self.variable as usize)?;
        view.variable(id)?.scalar_name(self.scalar as usize)
    }
}

/// The aggregate state a formal state candidate integrates and, per scalar
/// `k`, the source scalar its value projection equation equates it to.
///
/// The candidate appends `state[k] = value_k` and `der(state[k]) = successor_k`
/// for each selected coordinate and records `coordinates[k]` from the same
/// selection in the same order, so the map is a product of that construction,
/// never a reading of names or equations afterwards.
#[derive(Clone, Debug)]
pub struct PreparedStateCoordinates {
    pub(super) state: u32,
    pub(super) coordinates: Box<[PreparedStateCoordinate]>,
}

impl PreparedStateCoordinates {
    /// The generated aggregate state variable in `view`.
    pub fn state<'dae>(&self, view: dae::DaeView<'dae>) -> Option<dae::VariableId<'dae>> {
        view.variable_id(self.state as usize)
    }

    /// The source scalar of each state scalar, in state scalar order.
    pub fn coordinates(&self) -> &[PreparedStateCoordinate] {
        &self.coordinates
    }
}

/// The maps a reduced state selection issues with its finalized transformed
/// DAE. Rebuilds that keep every declaration ordinal carry them unchanged; a
/// system no reduced selection finalized carries neither.
#[derive(Clone, Debug, Default)]
pub struct ReducedSelectionMaps {
    pub(super) coordinates: Option<PreparedStateCoordinates>,
    pub(super) charts: Box<[PreparedReducedChart]>,
}
