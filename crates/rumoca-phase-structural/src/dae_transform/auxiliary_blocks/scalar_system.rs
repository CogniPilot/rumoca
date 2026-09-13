//! Square blocks of source-authored scalar equations, never tensor row views.

mod affine;

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use rumoca_ir_dae as dae;

use super::super::constraints::DifferentiationFacts;
use super::tensor_expression::TensorExpression;
use super::{AuxiliaryBlock, AuxiliarySystem};
use affine::{AffineExpression, AffineProof};

struct ScalarEquation {
    residual: u32,
    affine: AffineExpression,
}

pub(super) fn derive_systems(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
    blocks: &mut [Option<Arc<AuxiliaryBlock>>],
) {
    let mut proof = AffineProof::new(view, facts);
    let rows: Vec<_> = view
        .continuous_owners()
        .filter_map(|owner| {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                return None;
            };
            let residual = equation.residual();
            let affine = proof.expression(residual)?;
            (!affine.terms.is_empty()).then_some(ScalarEquation {
                residual: residual.index(),
                affine,
            })
        })
        .collect();
    let mut incidence = BTreeMap::<u32, Vec<usize>>::new();
    for (row, equation) in rows.iter().enumerate() {
        for &variable in equation.affine.terms.keys() {
            incidence.entry(variable).or_default().push(row);
        }
    }
    let mut remaining: BTreeSet<_> = incidence.keys().copied().collect();
    while let Some(first) = remaining.pop_first() {
        let (variables, component) = connected_rows(first, &rows, &incidence, &mut remaining);
        if variables.len() != component.len() {
            continue;
        }
        let block = source_block(view, facts, &rows, &variables, &component);
        if let Some(block) = block {
            let block = Arc::new(block);
            for variable in variables {
                blocks[variable as usize] = Some(Arc::clone(&block));
            }
        }
    }
}

fn connected_rows(
    first: u32,
    rows: &[ScalarEquation],
    incidence: &BTreeMap<u32, Vec<usize>>,
    remaining: &mut BTreeSet<u32>,
) -> (BTreeSet<u32>, BTreeSet<usize>) {
    let mut variables = BTreeSet::from([first]);
    let mut selected = BTreeSet::new();
    let mut pending = vec![first];
    while let Some(variable) = pending.pop() {
        for &row in &incidence[&variable] {
            if !selected.insert(row) {
                continue;
            }
            discover_variables(&rows[row], &mut variables, remaining, &mut pending);
        }
    }
    (variables, selected)
}

fn discover_variables(
    row: &ScalarEquation,
    variables: &mut BTreeSet<u32>,
    remaining: &mut BTreeSet<u32>,
    pending: &mut Vec<u32>,
) {
    for &variable in row.affine.terms.keys() {
        if variables.insert(variable) {
            remaining.remove(&variable);
            pending.push(variable);
        }
    }
}

fn source_block(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
    rows: &[ScalarEquation],
    variables: &BTreeSet<u32>,
    selected: &BTreeSet<usize>,
) -> Option<AuxiliaryBlock> {
    let matrix = TensorExpression::Array(
        selected
            .iter()
            .map(|&row| {
                TensorExpression::Array(
                    variables
                        .iter()
                        .map(|variable| {
                            rows[row]
                                .affine
                                .terms
                                .get(variable)
                                .cloned()
                                .unwrap_or_else(affine::zero)
                        })
                        .collect(),
                )
            })
            .collect(),
    );
    let rhs = TensorExpression::Array(
        selected
            .iter()
            .map(|&row| TensorExpression::Negate(Box::new(rows[row].affine.offset.clone())))
            .collect(),
    );
    let mut operands = Vec::new();
    matrix.operands(&mut operands);
    rhs.operands(&mut operands);
    let mut states = BTreeSet::new();
    for operand in operands {
        states.extend(facts.materialized_state_anchors(view, operand.expression)?);
    }
    Some(AuxiliaryBlock {
        variable: *variables.first()?,
        extent: u32::try_from(variables.len()).ok()?,
        system: AuxiliarySystem::Scalars {
            variables: variables.iter().copied().collect(),
            residuals: selected.iter().map(|&row| rows[row].residual).collect(),
            matrix,
            rhs,
        },
        state_anchors: states.into_iter().collect(),
    })
}
