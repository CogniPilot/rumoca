//! Evaluable-parameter folding (SPEC_0040 STRUCT-T10(a)).
//!
//! A parameter whose checked `evaluable` attribute is set (a `final` or
//! `Evaluate=true` parameter whose binding reads only constants and other such
//! parameters, MLS §4.5 and §18.6) is replaced by its value at every read, in
//! one checked reconstruction. The declaration and its binding stay, so the
//! parameter remains an exported, observable constant. Ordinary parameters are
//! never folded, and a binding the checked evaluator cannot settle leaves its
//! parameter unchanged.

use std::sync::Arc;

use rumoca_eval_dae::NumericEvaluator;
use rumoca_ir_dae as dae;

use crate::StructuralError;

/// The settled value of one folded parameter, in row-major scalar order.
#[derive(Clone, Debug, PartialEq)]
pub(super) struct FoldedValue {
    pub(super) scalar: dae::ScalarType,
    pub(super) dimensions: Box<[u32]>,
    pub(super) values: Box<[f64]>,
}

/// Fold every read evaluable parameter of `model`.
///
/// Returns `None` when no evaluable parameter is read, leaving the source
/// untouched.
pub fn fold_evaluable_parameters(model: &dae::Dae) -> Result<Option<dae::Dae>, StructuralError> {
    let plan = model.inspect(fold_plan);
    if plan.iter().all(Option::is_none) {
        return Ok(None);
    }
    super::reconstruction::rebuild_folded_parameters(model, &plan).map(Some)
}

/// One settled value per declaration ordinal, present only for a read
/// evaluable Real, Integer, or Boolean parameter with a finite value.
pub(super) fn fold_plan(view: dae::DaeView<'_>) -> Vec<Option<Arc<FoldedValue>>> {
    let read = read_parameters(view);
    let mut evaluator = NumericEvaluator::new(view);
    view.variables()
        .map(|(id, variable)| {
            if !read[id.index() as usize] || !variable.is_evaluable() {
                return None;
            }
            let value_type = variable.value_type();
            let scalar = value_type.scalar_type();
            if !matches!(
                scalar,
                dae::ScalarType::Real | dae::ScalarType::Integer | dae::ScalarType::Boolean
            ) || variable.scalar_count() == 0
            {
                return None;
            }
            let values = evaluator.initial_value(id).ok()?;
            let representable = values.len() == variable.scalar_count()
                && values.iter().all(|&value| representable(scalar, value));
            representable.then(|| {
                Arc::new(FoldedValue {
                    scalar,
                    dimensions: value_type.dimensions().into(),
                    values: values.into(),
                })
            })
        })
        .collect()
}

/// Whether one settled scalar has an exact literal of its declared type.
fn representable(scalar: dae::ScalarType, value: f64) -> bool {
    match scalar {
        dae::ScalarType::Real => value.is_finite(),
        dae::ScalarType::Integer => {
            value.fract() == 0.0 && value >= i64::MIN as f64 && value < i64::MAX as f64
        }
        dae::ScalarType::Boolean => value == 0.0 || value == 1.0,
        _ => false,
    }
}

/// Parameters read by at least one expression coordinate.
fn read_parameters(view: dae::DaeView<'_>) -> Vec<bool> {
    let mut read = vec![false; view.variable_count()];
    for index in 0..view.expression_count() {
        let Some(node) = view.expression_id(index).and_then(|id| view.expression(id)) else {
            continue;
        };
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) =
            node.operation()
        {
            read[id.index() as usize] = true;
        }
    }
    read
}

/// The literal of one folded value at a read's own provenance.
pub(super) fn folded_literal<'target>(
    target: &mut dae::Expressions<'_, 'target>,
    value: &FoldedValue,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
    shaped_literal(
        target,
        value.scalar,
        &value.dimensions,
        &value.values,
        provenance,
    )
}

fn shaped_literal<'target>(
    target: &mut dae::Expressions<'_, 'target>,
    scalar: dae::ScalarType,
    dimensions: &[u32],
    values: &[f64],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
    let Some((&extent, inner)) = dimensions.split_first() else {
        return target.at(provenance).literal(match scalar {
            dae::ScalarType::Integer => dae::DaeLiteral::Integer(values[0] as i64),
            dae::ScalarType::Boolean => dae::DaeLiteral::Boolean(values[0] != 0.0),
            _ => dae::DaeLiteral::Real(values[0]),
        });
    };
    let stride = values.len() / extent as usize;
    let elements = values
        .chunks_exact(stride)
        .map(|chunk| shaped_literal(target, scalar, inner, chunk, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    target.at(provenance).array(elements)
}
