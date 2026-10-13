//! Lexical predecessor projection from constructor-owned function regions.

use rumoca_ir_dae as dae;
use serde_json::{Value, json};

pub(super) fn project<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Vec<dae::FunctionAssignedState<'dae>> {
    let mut predecessors = vec![None; function.definition_count()];
    let mut current = vec![dae::FunctionAssignedState::Unassigned; function.values().count()];
    walk(
        view,
        function,
        function.statements(),
        &mut current,
        &mut predecessors,
    );
    predecessors
        .into_iter()
        .map(|state| state.expect("checked definition belongs to an ordered function owner"))
        .collect()
}

pub(super) fn project_state(state: dae::FunctionAssignedState<'_>) -> Value {
    match state {
        dae::FunctionAssignedState::Unassigned => json!({ "kind": "unassigned" }),
        dae::FunctionAssignedState::Assigned(id) => {
            json!({ "kind": "assigned", "definition": id.ordinal() })
        }
        dae::FunctionAssignedState::Conditional { owner, value } => json!({
            "kind": "conditional", "conditional": owner.ordinal(), "value": value.ordinal(),
        }),
    }
}

fn record<'dae>(
    definition: dae::FunctionDefinitionView<'dae>,
    current: &mut [dae::FunctionAssignedState<'dae>],
    predecessors: &mut [Option<dae::FunctionAssignedState<'dae>>],
) {
    let target = definition.target().ordinal() as usize;
    let slot = &mut predecessors[definition.id().ordinal() as usize];
    assert!(slot.is_none(), "checked definition has one lexical owner");
    *slot = Some(current[target]);
    current[target] = dae::FunctionAssignedState::Assigned(definition.id());
}

fn walk<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    statements: dae::FunctionStatements<'dae>,
    current: &mut [dae::FunctionAssignedState<'dae>],
    predecessors: &mut [Option<dae::FunctionAssignedState<'dae>>],
) {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assignment { definition }
            | dae::FunctionStatementView::RequireAssigned { definition, .. } => {
                record(definition, current, predecessors);
            }
            dae::FunctionStatementView::AssignmentGroup { definitions } => {
                for definition in definitions.iter() {
                    record(definition, current, predecessors);
                }
            }
            dae::FunctionStatementView::Conditional { conditional } => {
                walk_conditional(view, function, conditional, current, predecessors);
            }
            dae::FunctionStatementView::For {
                fold, statements, ..
            } => {
                walk_fold(view, function, fold, statements, current, predecessors);
            }
            dae::FunctionStatementView::Assertion { .. } => {}
        }
    }
}

fn walk_conditional<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    conditional: dae::FunctionConditionalBodyView<'dae>,
    current: &mut [dae::FunctionAssignedState<'dae>],
    predecessors: &mut [Option<dae::FunctionAssignedState<'dae>>],
) {
    for branch in conditional
        .branches()
        .chain(std::iter::once(conditional.fallback()))
    {
        let mut incoming = function
            .values()
            .map(|value| {
                branch
                    .incoming(value.id())
                    .expect("checked branch names its values")
            })
            .collect::<Vec<_>>();
        walk(
            view,
            function,
            branch.statements(),
            &mut incoming,
            predecessors,
        );
    }
    for definition in conditional.outputs() {
        record(definition, current, predecessors);
    }
    for value in function.values() {
        current[value.id().ordinal() as usize] = conditional
            .outgoing(value.id())
            .expect("checked conditional names its values");
    }
}

fn walk_fold<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    fold: dae::FunctionFoldId<'dae>,
    statements: dae::FunctionStatements<'dae>,
    current: &mut [dae::FunctionAssignedState<'dae>],
    predecessors: &mut [Option<dae::FunctionAssignedState<'dae>>],
) {
    let fold = view.function_fold(fold).expect("checked fold resolves");
    let mut body = current.to_vec();
    for local in fold.iteration_locals() {
        body[local.ordinal() as usize] = dae::FunctionAssignedState::Unassigned;
    }
    for definition in fold.parameter_values().iter() {
        record(definition, &mut body, predecessors);
    }
    walk(view, function, statements, &mut body, predecessors);
    // Fold outputs replace the enclosing incoming value. The last transition
    // definition belongs to the body and is named separately by update_values.
    for definition in fold.output_values().iter() {
        record(definition, current, predecessors);
    }
}
