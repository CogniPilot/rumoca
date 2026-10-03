use rumoca_ir_dae as dae;

pub(crate) fn expression_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    variable_clock: impl Fn(dae::VariableId<'dae>) -> Option<dae::ClockId<'dae>>,
) -> Option<dae::ClockId<'dae>> {
    let mut owner = None;
    dae::for_each_expression(view, expression, |_, node| {
        if owner.is_some() {
            return;
        }
        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            return;
        };
        if let dae::CoordinateView::Previous(previous) = coordinate {
            owner = Some(
                view.previous(previous)
                    .expect("checked previous identity")
                    .clock(),
            );
            return;
        }
        owner = super::coordinate_variable(coordinate)
            .or_else(|| super::pre_coordinate_variable(coordinate))
            .and_then(|index| view.variable_id(index as usize))
            .and_then(&variable_clock);
    });
    owner
}

/// The clock whose partition owns the relation `expression`, or `None` when it
/// is a continuous-time relation.
///
/// A relation of a clocked equation reads only its partition's values, which
/// change on ticks alone, so it is evaluated on those ticks and owns no root
/// (MLS §16.1). A relation that also reads `time` or an unclocked continuous
/// coordinate (a state, an algebraic, or an input) changes during integration
/// even when it reads a clocked value that is held between ticks (`time >=
/// t_i + t_width` with `t_i` written by `when sample(..)`), so MLS §8.5 makes
/// it an event-generating relation whose crossing must be located.
pub(crate) fn relation_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    variable_clock: impl Fn(dae::VariableId<'dae>) -> Option<dae::ClockId<'dae>>,
) -> Option<dae::ClockId<'dae>> {
    let owner = expression_clock_owner(view, expression, &variable_clock)?;
    let mut continuous = false;
    dae::for_each_expression(view, expression, |_, node| {
        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            return;
        };
        let variable = match coordinate {
            dae::CoordinateView::Time => {
                continuous = true;
                return;
            }
            dae::CoordinateView::State(id) | dae::CoordinateView::Derivative(id) => id.index(),
            dae::CoordinateView::Algebraic(id) => id.index(),
            dae::CoordinateView::Input(id) => id.index(),
            _ => return,
        };
        continuous |= view
            .variable_id(variable as usize)
            .is_some_and(|variable| variable_clock(variable).is_none());
    });
    (!continuous).then_some(owner)
}
