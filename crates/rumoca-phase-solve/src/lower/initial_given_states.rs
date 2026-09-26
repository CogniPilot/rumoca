//! State starts known before the simultaneous initialization solve.
//!
//! The prepared model supplies these coordinates, including legal FMI start
//! overrides. A start that depends on an initialization unknown remains an
//! equation; a seed cannot discharge that dependency. A start that reads a
//! settable parameter (MLS §4.5: a parameter that is neither a constant nor
//! evaluated at translation) is known before the solve but not at
//! translation: it is assigned from the parameter at initialization, so a set
//! of the parameter takes effect.

use super::initial_parameters::InitializationParameterOwnership;
use rumoca_ir_dae as dae;

/// How a fixed state's start is known before the initialization solve.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum StartKnowledge {
    /// Its value is fixed at translation: the prepared model seeds it.
    Translation,
    /// It reads a settable parameter: an initialization update assigns it.
    Parameters,
    /// It reads an initialization unknown: it stays an equation.
    Unknown,
}

pub(super) fn start_knowledge<'dae>(
    view: dae::DaeView<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
    start: Option<(dae::ExprId<'dae>, usize)>,
    cache: &mut rumoca_eval_dae::ScalarCoordinateProjectionCache<'dae>,
) -> StartKnowledge {
    let Some((expression, scalar)) = start else {
        return StartKnowledge::Translation;
    };
    let (mut known, mut settable) = (true, false);
    let result = rumoca_eval_dae::for_each_scalar_coordinate_cached(
        view,
        expression,
        scalar,
        None,
        cache,
        |coordinate, _| match coordinate {
            dae::CoordinateView::Parameter(parameter) => {
                known &= ownership
                    .projection_unknown_slots(parameter.index())
                    .is_none()
                    && ownership.substitution(parameter.index()).is_none();
                settable |=
                    view.variable(dae::VariableId::from(parameter))
                        .is_some_and(|variable| {
                            variable.role() == dae::VariableRole::Parameter
                                && !variable.is_evaluable()
                        });
            }
            _ => known = false,
        },
    );
    match (result.is_ok() && known, settable) {
        (false, _) => StartKnowledge::Unknown,
        (true, false) => StartKnowledge::Translation,
        (true, true) => StartKnowledge::Parameters,
    }
}
