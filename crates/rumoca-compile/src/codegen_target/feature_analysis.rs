//! Read-only feature discovery over the checked DAE.

use rumoca_ir_dae as dae;

pub(super) fn dae_has_external_functions(_: &dae::Dae) -> bool {
    // External lifecycle declarations are not part of the checked function
    // grammar. Phase DAE must reject them until that grammar owns their full
    // construction contract.
    false
}

pub(super) fn dae_uses_external_tables(model: &dae::Dae) -> bool {
    model.inspect(|view| {
        calls_named(view, |name| {
            matches!(
                rumoca_core::top_level_last_segment(name),
                "ExternalCombiTimeTable"
                    | "ExternalCombiTable1D"
                    | "ExternalCombiTable2D"
                    | "getTimeTableTmax"
                    | "getTimeTableTmin"
                    | "getTimeTableValueNoDer"
                    | "getTimeTableValueNoDer2"
                    | "getTimeTableValue"
                    | "getTable1DAbscissaUmax"
                    | "getTable1DAbscissaUmin"
                    | "getTable1DValueNoDer"
                    | "getTable1DValueNoDer2"
                    | "getTable1DValue"
                    | "getNextTimeEvent"
                    | "isValidTable"
            )
        })
    })
}

pub(super) fn dae_uses_random(model: &dae::Dae) -> bool {
    model.inspect(|view| {
        calls_named(view, |name| {
            let short = rumoca_core::top_level_last_segment(name);
            short.contains("Xorshift")
                || matches!(
                    short,
                    "initialState"
                        | "random"
                        | "impureRandom"
                        | "impureRandomInteger"
                        | "initializeImpureRandom"
                )
        })
    })
}

pub(super) fn dae_has_initialization(model: &dae::Dae) -> bool {
    model.inspect(|view| view.initialization_owner_count() != 0)
}

pub(super) fn dae_has_events(model: &dae::Dae) -> bool {
    model.inspect(|view| {
        view.condition_count() != 0
            || view.relation_count() != 0
            || view.root_count() != 0
            || view.time_event_count() != 0
            || view.event_action_count() != 0
            || view.discrete_real_equation_count() != 0
            || view.discrete_assignment_count() != 0
    })
}

pub(super) fn dae_has_runtime_events(model: &dae::Dae) -> bool {
    model.inspect(|view| view.terminal_count() != 0 || view.delay_count() != 0)
}

pub(super) fn dae_has_clocks(model: &dae::Dae) -> bool {
    model.inspect(|view| view.clock_count() != 0)
}

pub(super) const fn dae_has_unlowered_source_temporal_operators(_: &dae::Dae) -> bool {
    // The checked expression grammar has typed temporal coordinates and no
    // source temporal-call variant.
    false
}

pub(super) const fn dae_has_dynamic_ranges(_: &dae::Dae) -> bool {
    // Checked ranges store their integer start/step/stop values directly.
    false
}

pub(super) fn dae_has_dynamic_derivative_subscripts(model: &dae::Dae) -> bool {
    model.inspect(|view| {
        (0..view.expression_count()).any(|index| {
            let id = view
                .expression_id(index)
                .expect("finalized dense expression has an identity");
            let expression = view
                .expression(id)
                .expect("finalized expression identity resolves");
            let dae::ExpressionOperation::Index { base, subscripts } = expression.operation()
            else {
                return false;
            };
            let Some(base) = view.expression(base) else {
                return false;
            };
            matches!(
                base.operation(),
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(_))
            ) && subscripts
                .iter()
                .any(|subscript| dynamic_subscript(view, subscript))
        })
    })
}

fn dynamic_subscript<'dae>(view: dae::DaeView<'dae>, subscript: dae::SubscriptView<'dae>) -> bool {
    let dae::SubscriptView::Index { expression, .. } = subscript else {
        return true;
    };
    view.expression(expression).is_none_or(|expression| {
        !matches!(
            expression.operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(_))
        )
    })
}

fn calls_named(view: dae::DaeView<'_>, predicate: impl Fn(&str) -> bool) -> bool {
    (0..view.expression_count()).any(|index| {
        let id = view
            .expression_id(index)
            .expect("finalized dense expression has an identity");
        let expression = view
            .expression(id)
            .expect("finalized expression identity resolves");
        let dae::ExpressionOperation::Call { function, .. } = expression.operation() else {
            return false;
        };
        view.function(function)
            .is_some_and(|function| predicate(function.name().as_str()))
    })
}
