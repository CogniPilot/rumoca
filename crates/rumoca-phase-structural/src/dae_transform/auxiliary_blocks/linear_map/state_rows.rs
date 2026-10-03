//! Independent source equations defining a dependent whole-vector state.

use std::sync::Arc;

use rumoca_core::StateSelect;
use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

use super::super::super::constraints::DifferentiationFacts;
use super::super::tensor_expression::TensorExpression;
use super::super::{AuxiliaryBlock, AuxiliarySystem};
use super::affine::{AffineMap, AffineValue};
use super::materialized_sources::MaterializedSources;

struct SourceRow {
    residual: u32,
    coefficient: TensorExpression,
    rhs: TensorExpression,
}

pub(in crate::dae_transform) fn derive_state_blocks(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> Vec<Arc<AuxiliaryBlock>> {
    let mut sources = MaterializedSources::new(view, facts);
    let residuals = view
        .continuous_owners()
        .flat_map(super::source_residuals)
        .collect::<Vec<_>>();
    let relevance = ResidualRelevance::record(&mut sources, &residuals);
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
            let residuals = relevance.residuals_for(id.index(), &residuals);
            derive_block(&mut sources, id.index(), *extent, &residuals).map(Arc::new)
        })
        .collect()
}

/// Which residuals can yield a row for a given vector state.
///
/// One recording walk per residual (see [`AffineMap::recording`]) lists every
/// variable the residual's rows could depend on. A residual whose recording
/// walk yields no row yields none for any variable it never compared against,
/// so each state walks only the residuals that name it, in source order,
/// rather than every residual of the system.
struct ResidualRelevance {
    /// Residual ordinals by the variables their walks compared against.
    by_variable: std::collections::BTreeMap<u32, Vec<usize>>,
    /// Residuals whose recording walk already yields a row, relevant to every
    /// variable.
    unconditional: Vec<usize>,
}

impl ResidualRelevance {
    fn record<'dae>(
        sources: &mut MaterializedSources<'dae, '_>,
        residuals: &[dae::ExprId<'dae>],
    ) -> Self {
        let view = sources.view;
        let mut relevance = Self {
            by_variable: std::collections::BTreeMap::new(),
            unconditional: Vec::new(),
        };
        for (ordinal, &residual) in residuals.iter().enumerate() {
            let mut affine = AffineMap::recording(sources);
            let mut rows = Vec::new();
            collect_rows(view, &mut affine, residual, &mut rows);
            if !rows.is_empty() {
                relevance.unconditional.push(ordinal);
            }
            for variable in affine.into_tested() {
                relevance
                    .by_variable
                    .entry(variable)
                    .or_default()
                    .push(ordinal);
            }
        }
        relevance
    }

    fn residuals_for<'dae>(
        &self,
        variable: u32,
        residuals: &[dae::ExprId<'dae>],
    ) -> Vec<dae::ExprId<'dae>> {
        let mut ordinals = self
            .by_variable
            .get(&variable)
            .into_iter()
            .flatten()
            .chain(&self.unconditional)
            .copied()
            .collect::<Vec<_>>();
        ordinals.sort_unstable();
        ordinals.dedup();
        ordinals
            .into_iter()
            .map(|ordinal| residuals[ordinal])
            .collect()
    }
}

fn derive_block<'dae>(
    sources: &mut MaterializedSources<'dae, '_>,
    variable: u32,
    extent: u32,
    residuals: &[dae::ExprId<'dae>],
) -> Option<AuxiliaryBlock> {
    let view = sources.view;
    let mut affine = AffineMap::new(sources, variable, extent);
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
            anchors.extend(
                sources
                    .state_anchors(
                        view.expression_id(operand.expression as usize)?,
                        &operand.context(view),
                    )?
                    .iter()
                    .copied(),
            );
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
    affine: &mut AffineMap<'dae, '_, '_>,
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

/// For every variable of `view`: how many residuals its relevance keeps, how
/// many the system has, whether every residual a full walk draws a row from is
/// kept, and whether the kept residuals derive the same block as all of them.
#[cfg(test)]
pub(in crate::dae_transform) fn relevance_report(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> Vec<(usize, usize, bool, bool)> {
    let mut sources = MaterializedSources::new(view, facts);
    let residuals = view
        .continuous_owners()
        .flat_map(super::source_residuals)
        .collect::<Vec<_>>();
    let relevance = ResidualRelevance::record(&mut sources, &residuals);
    (0..view.variable_count() as u32)
        .map(|variable| {
            let kept = relevance.residuals_for(variable, &residuals);
            let covered = residuals.iter().all(|&residual| {
                let mut affine = AffineMap::new(&mut sources, variable, 3);
                let mut rows = Vec::new();
                collect_rows(view, &mut affine, residual, &mut rows);
                rows.is_empty() || kept.contains(&residual)
            });
            let kept_block = derive_block(&mut sources, variable, 3, &kept)
                .map(|block| (block.residual(), block.state_anchors));
            let full_block = derive_block(&mut sources, variable, 3, &residuals)
                .map(|block| (block.residual(), block.state_anchors));
            let same = kept_block == full_block;
            (kept.len(), residuals.len(), covered, same)
        })
        .collect()
}
