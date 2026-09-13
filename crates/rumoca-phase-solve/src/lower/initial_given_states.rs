//! State starts known before the simultaneous initialization solve.
//!
//! The prepared model supplies these coordinates, including legal FMI start
//! overrides. A start that depends on an initialization unknown remains an
//! equation; a seed cannot discharge that dependency.

use super::initial_parameters::InitializationParameterOwnership;
use rumoca_ir_dae as dae;

pub(super) fn is_known<'dae>(
    view: dae::DaeView<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
    start: Option<(dae::ExprId<'dae>, usize)>,
    cache: &mut rumoca_eval_dae::ScalarCoordinateProjectionCache<'dae>,
) -> bool {
    let Some((expression, scalar)) = start else {
        return true;
    };
    let mut known = true;
    let result = rumoca_eval_dae::for_each_scalar_coordinate_cached(
        view,
        expression,
        scalar,
        None,
        cache,
        |coordinate, _| {
            known &= match coordinate {
                dae::CoordinateView::Parameter(parameter) => {
                    ownership
                        .projection_unknown_slots(parameter.index())
                        .is_none()
                        && ownership.substitution(parameter.index()).is_none()
                }
                _ => false,
            };
        },
    );
    result.is_ok() && known
}
