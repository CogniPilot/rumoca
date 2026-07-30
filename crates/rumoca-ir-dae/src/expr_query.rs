//! Read-only queries over the checked expression arena.
//!
//! Queries use branded identities directly. They never recover variable
//! identity from display text and never need malformed-expression branches:
//! every operand was checked when its node entered the arena.

use crate::{
    CoordinateView, DaeView, ExprId, ExpressionOperation, ExpressionView, StateId, SubscriptView,
    VariableId,
};

/// Visit one expression DAG, including index and slice expressions.
///
/// Shared nodes are visited once. The root is visited before its operands.
pub fn for_each_expression<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    mut visit: impl FnMut(ExprId<'dae>, ExpressionView<'dae>),
) {
    let mut pending = vec![root];
    let mut visited = vec![false; dae.expression_count()];
    while let Some(expression) = pending.pop() {
        let index = expression.index() as usize;
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let node = dae
            .expression(expression)
            .expect("a branded expression identity resolves in its owning DAE");
        visit(expression, node);
        push_children(dae, node.operation(), &mut pending);
    }
}

/// True when any coordinate in `root` belongs to `variable`.
///
/// Current, derivative, `pre`, and role-specific coordinate forms all retain
/// the same declaration identity and therefore all count as uses.
pub fn expr_contains_var<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    variable: VariableId<'dae>,
) -> bool {
    expression_any(dae, root, |node| {
        node.variable_coordinate() == Some(variable)
    })
}

/// True when `root` itself denotes `variable`, optionally through indexing.
///
/// This intentionally does not search arithmetic operands. Use
/// [`expr_contains_var`] for a recursive occurrence query.
pub fn expr_refers_to_var<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    variable: VariableId<'dae>,
) -> bool {
    let node = dae
        .expression(root)
        .expect("a branded expression identity resolves in its owning DAE");
    if node.variable_coordinate() == Some(variable) {
        return true;
    }
    match node.operation() {
        ExpressionOperation::Index { base, .. } => expr_refers_to_var(dae, base, variable),
        _ => false,
    }
}

/// True when `root` contains the derivative coordinate of `state`.
pub fn expr_contains_der_of<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    state: StateId<'dae>,
) -> bool {
    expression_any(dae, root, |node| {
        matches!(
            node.operation(),
            ExpressionOperation::Coordinate(CoordinateView::Derivative(candidate))
                if candidate == state
        )
    })
}

/// True when `root` contains a derivative accepted by `matches`.
pub fn expr_contains_der_of_any<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    mut matches: impl FnMut(StateId<'dae>) -> bool,
) -> bool {
    expression_any(dae, root, |node| match node.operation() {
        ExpressionOperation::Coordinate(CoordinateView::Derivative(state)) => matches(state),
        _ => false,
    })
}

fn expression_any<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    mut predicate: impl FnMut(ExpressionView<'dae>) -> bool,
) -> bool {
    let mut pending = vec![root];
    let mut visited = vec![false; dae.expression_count()];
    while let Some(expression) = pending.pop() {
        let index = expression.index() as usize;
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let node = dae
            .expression(expression)
            .expect("a branded expression identity resolves in its owning DAE");
        if predicate(node) {
            return true;
        }
        push_children(dae, node.operation(), &mut pending);
    }
    false
}

fn push_children<'dae>(
    dae: DaeView<'dae>,
    operation: ExpressionOperation<'dae>,
    pending: &mut Vec<ExprId<'dae>>,
) {
    match operation {
        ExpressionOperation::Literal(_)
        | ExpressionOperation::Coordinate(_)
        | ExpressionOperation::FunctionFoldParameter { .. } => {}
        ExpressionOperation::Range(range) => {
            pending.push(range.stop().expression());
            if let Some(step) = range.explicit_step() {
                pending.push(step.expression());
            }
            pending.push(range.start().expression());
        }
        ExpressionOperation::Unary { operand, .. } => pending.push(operand),
        ExpressionOperation::Binary { lhs, rhs, .. } => {
            pending.push(rhs);
            pending.push(lhs);
        }
        ExpressionOperation::Conditional(operands)
        | ExpressionOperation::Array(operands)
        | ExpressionOperation::Record(operands)
        | ExpressionOperation::Builtin {
            arguments: operands,
            ..
        }
        | ExpressionOperation::Call {
            arguments: operands,
            ..
        } => pending.extend(operands.iter()),
        ExpressionOperation::Comprehension { body, .. } => pending.push(body),
        ExpressionOperation::Field { base, .. } => pending.push(base),
        ExpressionOperation::FunctionValue { definition, .. } => pending.push(definition.rhs()),
        ExpressionOperation::FunctionFoldOutput { fold, .. } => {
            let fold = dae
                .function_fold(fold)
                .expect("checked function fold identity resolves");
            pending.extend(fold.initial_values().rhs_iter());
            pending.extend(fold.update_values().rhs_iter());
        }
        ExpressionOperation::Index { base, subscripts } => {
            push_subscripts(subscripts, pending);
            pending.push(base);
        }
        ExpressionOperation::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            push_subscripts(subscripts, pending);
            pending.extend([base, value]);
        }
    }
}

fn push_subscripts<'dae>(subscripts: crate::SubscriptsView<'dae>, pending: &mut Vec<ExprId<'dae>>) {
    for subscript in subscripts.iter() {
        match subscript {
            SubscriptView::Index { expression, .. } | SubscriptView::Slice { expression, .. } => {
                pending.push(expression)
            }
            SubscriptView::Whole { .. } => {}
        }
    }
}
