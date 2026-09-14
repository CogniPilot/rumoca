//! Prove source tensor maps affine in one aggregate coordinate.

mod affine;
mod materialized_sources;
mod state_rows;

use super::super::constraints::DifferentiationFacts;
use super::tensor_expression::{SourceValue, TensorExpression};
use super::{AuxiliaryBlock, AuxiliarySystem, vector_unknown};
use affine::AffineMap;
use materialized_sources::MaterializedSources;
use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;
pub(in crate::dae_transform) use state_rows::derive_state_blocks;
use std::sync::Arc;

pub(super) fn derive_maps(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
    blocks: &mut [Option<Arc<AuxiliaryBlock>>],
) {
    let mut sources = MaterializedSources::new(view, facts);
    let mut traversal = dae::ExpressionTraversal::new();
    for residual in view.continuous_owners().flat_map(source_residuals) {
        derive_equation_map(&mut sources, residual, blocks, &mut traversal);
    }
}

fn derive_equation_map<'dae>(
    sources: &mut MaterializedSources<'dae, '_>,
    residual: dae::ExprId<'dae>,
    blocks: &mut [Option<Arc<AuxiliaryBlock>>],
    traversal: &mut dae::ExpressionTraversal<'dae>,
) {
    let view = sources.view;
    let facts = sources.facts;
    let node = view.expression(residual).unwrap();
    let [extent] = node.value_type().dimensions() else {
        return;
    };
    if *extent == 0 || node.binder_domain().is_some() {
        return;
    }
    let Some((lhs, rhs)) = crate::residual_normalization::equation_sides(view, residual) else {
        return;
    };
    for (map, value) in [(lhs, rhs), (rhs, lhs)] {
        let Some(anchors) = facts.materialized_state_anchors(view, value.index()) else {
            continue;
        };
        let mut candidates = Vec::new();
        traversal.visit_pruned(view, [map], |expression, _| {
            if let Some((variable, size)) = vector_unknown(view, expression)
                && size == *extent
                && blocks[variable as usize].is_none()
                && !facts.can_materialize_value(view, expression.index())
            {
                candidates.push(variable);
            }
            true
        });
        candidates.sort_unstable();
        candidates.dedup();
        for variable in candidates {
            let mut walk = AffineMap::new(sources, variable, *extent);
            let Some(affine) = walk.expression(map, &FunctionCallContext::default()) else {
                continue;
            };
            if !affine.offset.is_zero() {
                continue;
            }
            let Some(matrix) = affine.coefficient else {
                continue;
            };
            let rhs = TensorExpression::Source(SourceValue::model(value.index()));
            let mut leaves = Vec::new();
            matrix.operands(&mut leaves);
            rhs.operands(&mut leaves);
            let mut states = anchors.clone();
            for leaf in leaves {
                states.extend(
                    facts
                        .materialized_state_anchors_in_context(
                            view,
                            leaf.expression,
                            &leaf.context(view),
                        )
                        .expect("independent coefficient leaf"),
                );
            }
            states.sort_unstable();
            states.dedup();
            blocks[variable as usize] = Some(Arc::new(AuxiliaryBlock {
                variable,
                extent: *extent,
                system: AuxiliarySystem::Map {
                    residual: residual.index(),
                    matrix,
                    rhs,
                },
                state_anchors: states.into_boxed_slice(),
            }));
        }
    }
}

fn source_residuals(
    owner: dae::ContinuousOwnerView<'_>,
) -> Box<dyn Iterator<Item = dae::ExprId<'_>> + '_> {
    match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => {
            Box::new(std::iter::once(equation.residual()))
        }
        dae::ContinuousOwnerView::Structured { family, .. }
            if family.scalar_view() == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
        {
            Box::new(family.bodies().iter())
        }
        _ => Box::new(std::iter::empty()),
    }
}
