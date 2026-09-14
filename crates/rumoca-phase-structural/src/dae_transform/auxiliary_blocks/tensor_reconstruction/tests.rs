use std::sync::Arc;

use super::*;
use crate::dae_transform::auxiliary_blocks::create_functions;
use crate::dae_transform::constraints::DifferentiationFacts;
use crate::dae_transform::expressions::{RebuiltBaseIdentities, RebuiltIdentities, shaped_zero};
use rumoca_core::{SourceMap, Span};
use rumoca_eval_dae::NumericEvaluator;

#[test]
fn cancelled_primal_coefficients_retain_scalar_and_tensor_shapes() {
    for shape in [vec![], vec![3], vec![2, 3], vec![0, 3]] {
        let text = "model CancelledCoefficient equation 0 = 0; end CancelledCoefficient;";
        let mut sources = SourceMap::new();
        let id = sources.add("CancelledCoefficient.mo", text);
        let at = dae::DaeProvenance::source(Span::from_offsets(id, 0, text.len())).unwrap();
        let model = dae::Dae::construct(sources, |target| {
            target.expressions(|e| shaped_zero(e, &shape, at).map(|_| ()))
        })
        .unwrap();
        let (result, values) = reconstruct_cancellation(&model, &shape, at);
        assert_eq!(values.len(), 3);
        result.inspect(|view| {
            let mut evaluator = NumericEvaluator::new(view);
            for value in values {
                let residual = view.expression_id(value as usize).unwrap();
                assert_eq!(
                    view.expression(residual).unwrap().value_type().dimensions(),
                    shape
                );
                assert_eq!(
                    evaluator.expression(residual).unwrap(),
                    vec![0.0; shape.iter().map(|&n| n as usize).product()]
                );
            }
        });
    }
}

fn reconstruct_cancellation(
    model: &dae::Dae,
    shape: &[u32],
    at: dae::DaeProvenance,
) -> (dae::Dae, Vec<u32>) {
    let mut output_indices = Vec::new();
    let result = model.inspect(|source| {
        let facts = DifferentiationFacts::collect(source);
        let source_zero = source.expression_id(source.expression_count() - 1).unwrap();
        let shared = TensorExpression::Shared {
            source: SourceValue::model(source_zero.index()),
            variable: None,
            offset: true,
            value: Arc::new(TensorExpression::Zero(shape.into())),
        };
        let cancelled = TensorExpression::Negate(Box::new(TensorExpression::Sum(
            dae::BinaryOperator::Subtract,
            Box::new(shared.clone()),
            Box::new(shared),
        )));
        dae::Dae::construct(model.source_map().clone(), |target| {
            let auxiliary_functions = create_functions(source, target, &facts)?;
            let values = target.expressions(|expressions| {
                let base = RebuiltBaseIdentities {
                    auxiliary_functions: &auxiliary_functions,
                    types: &[],
                    variables: &[],
                    domains: &[],
                    conditions: &[],
                    clocks: &[],
                    previous: &[],
                    terminals: &[],
                };
                let mut rebuilt = vec![None; source.expression_count()];
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    RebuiltIdentities {
                        base,
                        functions: &[],
                    },
                    &facts,
                    None,
                    &mut rebuilt,
                );
                (0..=2)
                    .map(|order| rebuilder.tensor_coefficient(&cancelled, order, at))
                    .collect::<Result<Vec<_>, _>>()
            })?;
            output_indices.extend(values.iter().map(|value| value.index()));
            Ok(())
        })
        .unwrap()
    });
    (result, output_indices)
}
