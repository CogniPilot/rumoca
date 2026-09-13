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
