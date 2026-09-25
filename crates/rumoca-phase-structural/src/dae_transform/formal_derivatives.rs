//! Coupled differentiated equations, prior to independent-state construction.

use std::collections::BTreeSet;

use rumoca_ir_dae as dae;

use super::variables::ReservedVariable;
use crate::StructuralError;
use crate::differential_structure::analyze_differential_structure_with_order_bounds;

mod stage_reads;
mod stages;
mod state_candidates;
pub(super) use stages::EquationProlongation;
pub use stages::{FormalDerivativeStage, FormalStageCoordinate, FormalStageEquation};
pub(super) use state_candidates::SelectedCoordinate;
pub use state_candidates::{
    FormalStateCandidate, FormalStateCandidateView, FormalStateCoordinate, ReducedSelectionChart,
    StateSelection,
};

/// An inspectable prolongation of one source DAE, not an executable state basis.
pub struct FormalDerivativeSystem<'source> {
    source: &'source dae::Dae,
    model: dae::Dae,
    coordinates: Vec<Vec<u32>>,
    equations: Vec<EquationProlongation>,
    dimension: usize,
    source_pins: Vec<super::InitialValuePin>,
}

/// Both source and constructed coordinate identities stay bound to their roots.
#[derive(Clone, Copy)]
pub struct FormalDerivativeView<'map, 'source, 'target> {
    pub source: dae::DaeView<'source>,
    pub view: dae::DaeView<'target>,
    /// Exact initial-value transfers, indexed only in `source`.
    pub source_pins: &'map [super::InitialValuePin],
    coordinates: &'map [Vec<u32>],
    equations: &'map [EquationProlongation],
    dimension: usize,
}

impl FormalDerivativeSystem<'_> {
    pub fn inspect<R>(
        &self,
        inspect: impl for<'source, 'target> FnOnce(FormalDerivativeView<'_, 'source, 'target>) -> R,
    ) -> R {
        self.source.inspect(|source| {
            self.model.inspect(|view| {
                inspect(FormalDerivativeView {
                    source,
                    view,
                    source_pins: &self.source_pins,
                    coordinates: &self.coordinates,
                    equations: &self.equations,
                    dimension: self.dimension,
                })
            })
        })
    }
}

impl<'source, 'target> FormalDerivativeView<'_, 'source, 'target> {
    /// Order zero denotes the unchanged source value; higher orders are tensors
    /// of formal derivatives with the same shape.
    pub fn coordinate(
        &self,
        variable: dae::VariableId<'source>,
        order: usize,
    ) -> Option<dae::VariableId<'target>> {
        self.coordinates[variable.index() as usize]
            .get(order)
            .and_then(|&id| self.view.variable_id(id as usize))
    }

    pub fn formal_dimension(&self) -> usize {
        self.dimension
    }
}

/// Preserve the source system and append formal derivatives at certified orders.
/// Regular independent coordinates and executable reconstruction are still needed.
///
/// Offsets start from the source signatures. When a prolonged owner reads a
/// coordinate above its own stage (a supplied derivative's `noDerivative`
/// input rate, see the `stage_reads` module), that variable's order is raised and the
/// offsets are certified again, so the read becomes a dependency its stage
/// determines. Each round strictly raises a bound and the shared
/// differentiation profile caps orders, so the refinement terminates; a read
/// no raise can place is refused.
pub fn construct_formal_derivatives(
    model: &dae::Dae,
) -> Result<FormalDerivativeSystem<'_>, StructuralError> {
    model.inspect(|source| {
        let invariance = crate::time_invariant::TimeInvariance::derive(source);
        let mut order_bounds = vec![0_u32; source.variables().count()];
        loop {
            let analysis = analyze_differential_structure_with_order_bounds(source, &order_bounds)?;
            let offsets = analysis.tensor_offsets(source)?.ok_or_else(|| {
                StructuralError::UnspannedContractViolation {
                    reason:
                        "formal derivative construction requires compatible whole-tensor orders"
                            .into(),
                }
            })?;
            let mut orders = vec![0; source.variables().count()];
            for (coordinate, &order) in analysis.variables().iter().zip(offsets.variable_orders()) {
                orders[coordinate.variable().index() as usize] = order;
            }
            let (rebuilt, coordinates, equations) = super::reconstruction::rebuild_formal(
                model,
                source,
                &orders,
                offsets.equation_orders(),
            )?;
            let reads = rebuilt.inspect(|formal| {
                stage_reads::later_stage_reads(
                    formal,
                    &coordinates,
                    &equations,
                    invariance.algebraics(),
                )
            })?;
            if reads.is_empty() {
                return Ok(FormalDerivativeSystem {
                    source: model,
                    model: rebuilt,
                    coordinates,
                    equations,
                    dimension: analysis.formal_dimension(),
                    source_pins: super::transferred_initial_values(source)?,
                });
            }
            if !raise_order_bounds(&mut order_bounds, reads) {
                return Err(StructuralError::UnspannedContractViolation {
                    reason: "a prolonged equation reads a coordinate that no certified stage \
                             order determines"
                        .into(),
                });
            }
        }
    })
}

/// Raise each bound to the order a later-stage read needs; `false` when no
/// bound rises, so another certification round cannot place the reads.
fn raise_order_bounds(
    order_bounds: &mut [u32],
    reads: std::collections::BTreeMap<usize, u32>,
) -> bool {
    let mut raised = false;
    for (variable, order) in reads {
        let bound = &mut order_bounds[variable];
        raised |= order > *bound;
        *bound = (*bound).max(order);
    }
    raised
}

pub(super) fn reserve_derivatives<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    types: &[dae::ValueTypeId<'target>],
    variables: &mut [ReservedVariable<'target>],
    orders: &[u32],
) -> Result<(), dae::DaeConstructionError> {
    let mut names = source.variables().map(|(_, v)| v.name().clone()).collect();
    for ((id, variable), reserved) in source.variables().zip(variables) {
        let provenance = dae::DaeProvenance::generated(
            dae::DaeGeneration::IndexReduction,
            variable.declaration().span(),
        )?;
        for order in 1..=orders[id.index() as usize] {
            let name = available_name(variable.name(), order, &mut names);
            let start = target.expressions(|expressions| {
                super::expressions::shaped_zero(
                    expressions,
                    variable.value_type().dimensions(),
                    provenance,
                )
            })?;
            let derivative = target.variables(|target| {
                let (id, reservation) = target.reserve_algebraic(
                    name,
                    types[variable.value_type_id().index() as usize],
                    provenance,
                )?;
                target.define(
                    reservation,
                    dae::VariableAttributes {
                        start: Some(start),
                        fixed: Some(vec![false]),
                        origin: dae::VariableOrigin::Generated,
                        ..Default::default()
                    },
                    provenance,
                )?;
                Ok(id)
            })?;
            reserved.formal_derivatives.push(derivative);
        }
        if variable.role() == dae::VariableRole::State {
            reserved.derivative_alias = reserved.formal_derivatives.first().copied();
            if reserved.derivative_alias.is_none() {
                return Err(dae::DaeConstructionError::IncompleteDefinition {
                    kind: "formal derivative for source state",
                    index: id.index(),
                    span: variable.declaration().span(),
                });
            }
        }
    }
    Ok(())
}

fn available_name(
    source: &rumoca_core::VarName,
    order: u32,
    names: &mut BTreeSet<rumoca_core::VarName>,
) -> rumoca_core::VarName {
    let base = format!("$formal_derivative.{order}.{source}");
    let mut candidate = rumoca_core::VarName::new(&base);
    let mut suffix = 0;
    while !names.insert(candidate.clone()) {
        suffix += 1;
        candidate = rumoca_core::VarName::new(format!("{base}.{suffix}"));
    }
    candidate
}
