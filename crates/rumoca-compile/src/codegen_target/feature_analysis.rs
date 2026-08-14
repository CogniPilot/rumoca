//! Read-only feature discovery over the checked DAE.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

/// Whether the checked DAE carries any MLS §12.9 external function interface.
///
/// SEV-155: this probe used to return a hard-coded `false`, so the
/// `external_functions == Some(false)` gate in
/// [`super::validate_dae_target_capabilities`] could not fire at all. Phase DAE
/// *does* construct external bodies (`FunctionBodyEntry::External`, built by
/// `define_external`), so the checked function table is exactly the thing to
/// interrogate, and the walk below reports it.
///
/// There is no "unknown" arm to fail closed on, because a finalized `Dae`
/// admits no unreadable function. Every identity below `function_count()`
/// resolves by the dense-arena construction of `DaeView::function_id` and
/// `DaeView::function`, and a function without a body cannot reach a finalized
/// `Dae` at all: `DaeConstruction::function` runs `expect_complete_function`
/// and `finish_construction` rejects any leftover reservation with
/// `DaeConstructionError::IncompleteDefinition`. That last invariant is pinned
/// by `tests::an_undefined_function_cannot_reach_a_finalized_dae`, which is
/// what licenses `FunctionView::is_external` to assert it rather than this
/// caller re-deciding it.
///
/// SEV-156 keeps the reject arm total: there is no invoke/effect grammar for an
/// external call yet, so every external function must reject at the capability
/// boundary.
pub(super) fn dae_has_external_functions(model: &dae::Dae) -> bool {
    model.inspect(|view| {
        (0..view.function_count()).any(|index| {
            let id = view
                .function_id(index)
                .expect("dense checked function identity resolves");
            let function = view.function(id).expect("checked function resolves");
            function.is_external()
        })
    })
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
            || view.structured_root_count() != 0
            || view.time_event_count() != 0
            || view.event_action_count() != 0
            || view.discrete_real_equation_count() != 0
            || view.discrete_value_owner_count() != 0
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

pub(super) fn solve_has_events(problem: &solve::SolveProblem) -> bool {
    let discrete = &problem.discrete;
    let events = &problem.events;
    !discrete.event_iteration_plan.runs.is_empty()
        || !discrete.runtime_assignment_rhs.is_empty()
        || !discrete.post_commit_assignment_rhs.is_empty()
        || !discrete.rhs.is_empty()
        || !discrete.structured_rhs.is_empty()
        || !discrete.structured_updates.is_empty()
        || !events.root_conditions.is_empty()
        || !events.condition_memory_parameter_indices.is_empty()
        || !events.scheduled_root_conditions.is_empty()
        || !events.scheduled_time_events.is_empty()
        || !events.dynamic_time_event_names.is_empty()
        || !events.dynamic_time_event_rhs.is_empty()
        || !events.action_conditions.is_empty()
        || !events.actions.is_empty()
}

pub(super) fn solve_has_runtime_events(problem: &solve::SolveProblem) -> bool {
    let events = &problem.events;
    let delays = &events.delays;
    events.has_terminal_event
        || !delays.source_rhs.is_empty()
        || !delays.delay_time_rhs.is_empty()
        || !delays.delay_max_rhs.is_empty()
        || !delays.value_parameter_indices.is_empty()
}

pub(super) fn solve_has_clocks(problem: &solve::SolveProblem) -> bool {
    !problem.clocks.periodic_event_schedules.is_empty()
        || !problem.clocks.activation_parameter_indices.is_empty()
        || problem.discrete.clock_owners.iter().any(Option::is_some)
        || problem
            .discrete
            .structured_updates
            .iter()
            .any(|update| update.clock_owner.is_some())
}

pub(super) fn solve_has_initialization(problem: &solve::SolveProblem) -> bool {
    let initialization = &problem.initialization;
    !initialization.residual.is_empty()
        || !initialization.projection_unknowns.is_empty()
        || !initialization.projection_plan.is_empty()
        || !initialization.update_rhs.is_empty()
        || !initialization.update_targets.is_empty()
}

pub(super) fn solve_requires_residual_equations(
    problem: &solve::SolveProblem,
    exact_algebraic_assignments: Option<bool>,
) -> bool {
    let continuous = &problem.continuous;
    let algebraic_count = problem.solve_layout.algebraic_scalar_count();
    let has_algebraic_system = !continuous.implicit_rhs.is_empty()
        || !continuous.algebraic_projection_plan.is_empty()
        || algebraic_count != 0;
    has_algebraic_system
        && (exact_algebraic_assignments != Some(true)
            || !rumoca_phase_codegen::explicit_algebraic_assignment_complete(problem))
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
