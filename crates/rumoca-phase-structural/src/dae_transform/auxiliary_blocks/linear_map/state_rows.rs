//! Independent source equations defining a dependent whole-vector state.

use std::sync::Arc;

use rumoca_core::StateSelect;
use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

use super::super::super::constraints::DifferentiationFacts;
use super::super::tensor_expression::TensorExpression;
use super::super::{AuxiliaryBlock, AuxiliarySystem};
use super::affine::{AffineMap, AffineValue};

struct SourceRow {
    residual: u32,
    coefficient: TensorExpression,
    rhs: TensorExpression,
}

pub(in crate::dae_transform) fn derive_state_blocks(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> Vec<Arc<AuxiliaryBlock>> {
    let residuals = view
        .continuous_owners()
        .flat_map(super::source_residuals)
        .collect::<Vec<_>>();
    view.variables()
        .filter_map(|(id, variable)| {
            let [extent] = variable.value_type().dimensions() else {
                return None;
            };
            if variable.role() != dae::VariableRole::State
                || variable.state_select() == StateSelect::Always
                || variable.value_type().scalar_type() != dae::ScalarType::Real
                || *extent == 0
            {
                return None;
            }
            derive_block(view, facts, id.index(), *extent, &residuals).map(Arc::new)
        })
        .collect()
}

fn derive_block<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    variable: u32,
    extent: u32,
    residuals: &[dae::ExprId<'dae>],
) -> Option<AuxiliaryBlock> {
    let mut affine = AffineMap::new(view, facts, variable, extent);
    let mut rows = Vec::new();
    for &residual in residuals {
        collect_rows(view, &mut affine, residual, &mut rows);
    }
    if rows.len() != extent as usize {
        return None;
    }
    let mut anchors = Vec::new();
    for row in &rows {
        let mut operands = Vec::new();
        row.coefficient.operands(&mut operands);
        row.rhs.operands(&mut operands);
        for operand in operands {
            anchors.extend(facts.materialized_state_anchors_in_context(
                view,
                operand.expression,
                &operand.context(view),
            )?);
        }
    }
    if anchors.contains(&variable) {
        return None;
    }
    anchors.sort_unstable();
    anchors.dedup();
    let owners = rows.iter().map(|r| r.residual).collect();
    let (matrix, rhs): (Vec<_>, Vec<_>) = rows.into_iter().map(|r| (r.coefficient, r.rhs)).unzip();
    Some(AuxiliaryBlock {
        variable,
        extent,
        system: AuxiliarySystem::VectorRows {
            residuals: owners,
            matrix: TensorExpression::Array(matrix.into_boxed_slice()),
            rhs: TensorExpression::Array(rhs.into_boxed_slice()),
        },
        state_anchors: anchors.into_boxed_slice(),
    })
}

fn collect_rows<'dae>(
    view: dae::DaeView<'dae>,
    affine: &mut AffineMap<'dae, '_>,
    residual: dae::ExprId<'dae>,
    rows: &mut Vec<SourceRow>,
) {
    let node = view.expression(residual).unwrap();
    if node.binder_domain().is_some() {
        return;
    }
    let context = FunctionCallContext::default();
    if node.value_type().is_scalar() {
        if affine.is_value_definition(residual) {
            return;
        }
        if let Some(value) = affine.expression(residual, &context)
            && let Some(coefficient) = value.coefficient
        {
            rows.push(SourceRow {
                residual: residual.index(),
                coefficient,
                rhs: TensorExpression::Negate(Box::new(value.offset)),
            });
        }
        return;
    }
    let Some((lhs, rhs)) = crate::residual_normalization::equation_sides(view, residual) else {
        return;
    };
    for (vector, literal) in [(lhs, rhs), (rhs, lhs)] {
        let Some((elements, negated)) = signed_literal_elements(view, literal) else {
            continue;
        };
        let Some(value) = affine.expression(vector, &context) else {
            continue;
        };
        let Some(matrix) = value.coefficient else {
            continue;
        };
        for (ordinal, element) in elements.iter().enumerate() {
            if !view.expression(element).unwrap().value_type().is_scalar() {
                continue;
            }
            let Some(AffineValue {
                coefficient: None,
                offset,
            }) = affine.expression(element, &context)
            else {
                continue;
            };
            let offset = if negated {
                TensorExpression::Negate(Box::new(offset))
            } else {
                offset
            };
            rows.push(SourceRow {
                residual: residual.index(),
                coefficient: TensorExpression::Projection(Box::new(matrix.clone()), ordinal as u32),
                rhs: TensorExpression::Sum(
                    dae::BinaryOperator::Subtract,
                    Box::new(offset),
                    Box::new(TensorExpression::Projection(
                        Box::new(value.offset.clone()),
                        ordinal as u32,
                    )),
                ),
            });
        }
    }
}

fn signed_literal_elements<'dae>(
    view: dae::DaeView<'dae>,
    mut expression: dae::ExprId<'dae>,
) -> Option<(dae::ExpressionOperands<'dae>, bool)> {
    let mut negated = false;
    loop {
        match view.expression(expression)?.operation() {
            dae::ExpressionOperation::Array(elements) => return Some((elements, negated)),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus,
                operand,
            } => expression = operand,
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Negate,
                operand,
            } => {
                expression = operand;
                negated = !negated;
            }
            _ => return None,
        }
    }
}
