//! Source-owner activity checks shared by structural reconstruction candidates.

use rumoca_ir_dae as dae;

pub(super) fn preserves_equations(source: &dae::Dae, rebuilt: &dae::Dae) -> bool {
    source.inspect(|source| {
        rebuilt.inspect(|rebuilt| {
            let mut target = rebuilt.continuous_owners();
            source.continuous_owners().all(|owner| {
                target
                    .next()
                    .is_some_and(|replacement| preserves_owner(source, rebuilt, owner, replacement))
            }) && target.next().is_none()
        })
    })
}

fn preserves_owner<'source, 'target>(
    source: dae::DaeView<'source>,
    target: dae::DaeView<'target>,
    owner: dae::ContinuousOwnerView<'source>,
    replacement: dae::ContinuousOwnerView<'target>,
) -> bool {
    match (owner, replacement) {
        (
            dae::ContinuousOwnerView::Residual { equation, .. },
            dae::ContinuousOwnerView::Residual {
                equation: rebuilt, ..
            },
        ) => preserves_residual(source, target, equation.residual(), rebuilt.residual()),
        (
            dae::ContinuousOwnerView::Structured { family, .. },
            dae::ContinuousOwnerView::Structured {
                family: rebuilt, ..
            },
        ) => {
            family.bodies().len() == rebuilt.bodies().len()
                && family.bodies().iter().zip(rebuilt.bodies().iter()).all(
                    |(source_id, target_id)| {
                        preserves_residual(source, target, source_id, target_id)
                    },
                )
        }
        _ => false,
    }
}

fn preserves_residual<'source, 'target>(
    source: dae::DaeView<'source>,
    target: dae::DaeView<'target>,
    original: dae::ExprId<'source>,
    rebuilt: dae::ExprId<'target>,
) -> bool {
    !reflexive_coordinate_equality(target, rebuilt)
        || reflexive_coordinate_equality(source, original)
}

fn reflexive_coordinate_equality<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let Some((lhs, rhs)) = crate::residual_normalization::equation_sides(view, expression) else {
        return false;
    };
    let lhs = view.expression(lhs).expect("checked equality operand");
    let rhs = view.expression(rhs).expect("checked equality operand");
    lhs.value_type().scalar_type() == dae::ScalarType::Real
        && matches!((lhs.operation(), rhs.operation()),
            (dae::ExpressionOperation::Coordinate(lhs), dae::ExpressionOperation::Coordinate(rhs)) if lhs == rhs)
}
