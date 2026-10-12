//! Evaluated dependency projection over the unchanged checked source DAG.

use rumoca_ir_dae as dae;

use crate::FunctionCallContext;

/// Visit values needed by `root`, excluding only proved unselected branches.
///
/// Selection uses the existing exact literal/constant branch proof. Tunable
/// parameter defaults and runtime guards stay unknown, retaining every source
/// operand. This does not erase source arms, expand tensors, or enter calls.
/// Use the IR's full traversal for source/provenance queries.
pub fn for_each_selected_value_expression<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    visit: impl FnMut(dae::ExprId<'dae>, dae::ExpressionView<'dae>),
) {
    let context = FunctionCallContext::default();
    dae::ExpressionTraversal::new().visit_projected(
        view,
        [root],
        |expression, _| context.selected_branch(view, expression),
        visit,
    );
}
