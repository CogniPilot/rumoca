//! Source-owned affine tensor blocks available to index reduction.

mod linear_map;
mod reconstruction;
mod scalar_system;
mod tensor_expression;
mod tensor_reconstruction;

use std::collections::BTreeMap;
use std::sync::Arc;

use rumoca_ir_dae as dae;

use super::constraints::DifferentiationFacts;

pub(super) use reconstruction::{AuxiliaryExpression, AuxiliaryFunctions, create_functions};
use tensor_expression::{SourceValue, TensorExpression};

/// One source-authored scalar dot equation, `unknown * coefficient = rhs`.
/// Neither the vector entries nor its scalar equation views become owners.
#[derive(Clone)]
pub(super) struct DotEquation {
    pub(super) residual: u32,
    pub(super) coefficient: SourceValue,
    pub(super) rhs: SourceValue,
}

#[derive(Clone)]
enum AuxiliarySystem {
    DotRows(Box<[DotEquation]>),
    Map {
        residual: u32,
        matrix: TensorExpression,
        rhs: SourceValue,
    },
    Scalars {
        variables: Box<[u32]>,
        residuals: Box<[u32]>,
        matrix: TensorExpression,
        rhs: TensorExpression,
    },
}

/// A square source block whose coefficient and RHS values have independent
/// state/invariant definitions. Its runtime domain is the nonsingular matrix
/// domain enforced by the checked aggregate solve.
#[derive(Clone)]
pub(super) struct AuxiliaryBlock {
    pub(super) variable: u32,
    pub(super) extent: u32,
    system: AuxiliarySystem,
    pub(super) state_anchors: Box<[u32]>,
}

impl AuxiliaryBlock {
    #[cfg(test)]
    pub(super) fn coefficient_node_count(&self) -> usize {
        match &self.system {
            AuxiliarySystem::DotRows(rows) => rows.len(),
            AuxiliarySystem::Map { matrix, .. } => matrix.node_count(),
            AuxiliarySystem::Scalars { matrix, rhs, .. } => matrix.node_count() + rhs.node_count(),
        }
    }

    pub(super) fn contains_residual(&self, residual: u32) -> bool {
        match &self.system {
            AuxiliarySystem::DotRows(rows) => rows.iter().any(|row| row.residual == residual),
            AuxiliarySystem::Map {
                residual: owner, ..
            } => *owner == residual,
            AuxiliarySystem::Scalars { residuals, .. } => residuals.contains(&residual),
        }
    }

    pub(super) fn residual(&self) -> u32 {
        match &self.system {
            AuxiliarySystem::DotRows(rows) => rows[0].residual,
            AuxiliarySystem::Map { residual, .. } => *residual,
            AuxiliarySystem::Scalars { residuals, .. } => residuals[0],
        }
    }

    pub(super) fn operands(&self) -> std::vec::IntoIter<&SourceValue> {
        let mut operands = Vec::new();
        match &self.system {
            AuxiliarySystem::DotRows(rows) => {
                for row in rows {
                    operands.extend([&row.coefficient, &row.rhs]);
                }
            }
            AuxiliarySystem::Map { matrix, rhs, .. } => {
                matrix.operands(&mut operands);
                operands.push(rhs);
            }
            AuxiliarySystem::Scalars { matrix, rhs, .. } => {
                matrix.operands(&mut operands);
                rhs.operands(&mut operands);
            }
        }
        operands.into_iter()
    }
}

struct CandidateBlock {
    variable_expression: u32,
    extent: u32,
    rows: Vec<DotEquation>,
}

pub(super) fn derive_blocks(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> Vec<Option<Arc<AuxiliaryBlock>>> {
    let mut candidates = BTreeMap::<u32, CandidateBlock>::new();
    for owner in view.continuous_owners() {
        let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
            continue;
        };
        collect_equation(view, equation.residual(), &mut candidates);
    }
    let mut blocks = vec![None; view.variable_count()];
    for (variable, candidate) in candidates {
        if candidate.rows.len() != candidate.extent as usize
            || facts.can_materialize_value(view, candidate.variable_expression)
        {
            continue;
        }
        let Some(mut state_anchors) =
            candidate
                .rows
                .iter()
                .try_fold(Vec::new(), |mut states, row| {
                    states.extend(
                        facts.materialized_state_anchors(view, row.coefficient.expression)?,
                    );
                    states.extend(facts.materialized_state_anchors(view, row.rhs.expression)?);
                    Some(states)
                })
        else {
            continue;
        };
        state_anchors.sort_unstable();
        state_anchors.dedup();
        blocks[variable as usize] = Some(Arc::new(AuxiliaryBlock {
            variable,
            extent: candidate.extent,
            system: AuxiliarySystem::DotRows(candidate.rows.into_boxed_slice()),
            state_anchors: state_anchors.into_boxed_slice(),
        }));
    }
    linear_map::derive_maps(view, facts, &mut blocks);
    scalar_system::derive_systems(view, facts, &mut blocks);
    blocks
}

fn collect_equation<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
    candidates: &mut BTreeMap<u32, CandidateBlock>,
) {
    let Some(node) = view.expression(residual) else {
        return;
    };
    if !node.value_type().is_scalar() || node.binder_domain().is_some() {
        return;
    }
    let Some((lhs, rhs)) = crate::residual_normalization::equation_sides(view, residual) else {
        return;
    };
    for (dot, value) in [(lhs, rhs), (rhs, lhs)] {
        collect_dot(view, residual, dot, value, candidates);
    }
}

fn collect_dot<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
    dot: dae::ExprId<'dae>,
    value: dae::ExprId<'dae>,
    candidates: &mut BTreeMap<u32, CandidateBlock>,
) {
    let Some(dot) = view.expression(dot) else {
        return;
    };
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Multiply,
        lhs,
        rhs,
    } = dot.operation()
    else {
        return;
    };
    for (unknown, coefficient) in [(lhs, rhs), (rhs, lhs)] {
        let Some((variable, extent)) = vector_unknown(view, unknown) else {
            continue;
        };
        let coefficient_type = view.expression(coefficient).unwrap().value_type();
        if coefficient_type.scalar_type() != dae::ScalarType::Real
            || coefficient_type.dimensions() != [extent]
        {
            continue;
        }
        candidates
            .entry(variable)
            .or_insert_with(|| CandidateBlock {
                variable_expression: unknown.index(),
                extent,
                rows: Vec::new(),
            })
            .rows
            .push(DotEquation {
                residual: residual.index(),
                coefficient: SourceValue::model(coefficient.index()),
                rhs: SourceValue::model(value.index()),
            });
    }
}

fn vector_unknown<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<(u32, u32)> {
    let node = view.expression(expression)?;
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) =
        node.operation()
    else {
        return None;
    };
    let variable_view = view.variable(variable.into())?;
    let [extent] = node.value_type().dimensions() else {
        return None;
    };
    (node.value_type().scalar_type() == dae::ScalarType::Real
        && variable_view.variability() == dae::ExpressionVariability::Continuous
        && *extent > 0)
        .then_some((variable.index(), *extent))
}
