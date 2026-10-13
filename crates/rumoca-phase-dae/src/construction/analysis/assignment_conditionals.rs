//! Reuse selected statement ownership for a complete conditional assignment RHS.
use super::{FunctionStatementPlan, SelectedBooleanCapture};
use rumoca_core::{
    Expression, ExpressionRewriter, Span, Statement, StatementBlock, StatementRewriter, VarName,
};

/// Rewrite each conditional assignment into selected statements, then issue
/// every pure conditional as if-expression assignments: the ordered statement
/// form is kept only where assertions, partial definitions or loops need it.
pub(super) fn normalize(statements: &[Statement], function_values: &[VarName]) -> Vec<Statement> {
    let ordered = ConditionalAssignments.rewrite_statements(statements);
    super::pure_conditionals::express(
        &ordered,
        &mut super::pure_conditionals::Scope::new(function_values),
    )
}

struct ConditionalAssignments;
impl ExpressionRewriter for ConditionalAssignments {}
impl StatementRewriter for ConditionalAssignments {
    fn rewrite_statement(&mut self, statement: &Statement) -> Statement {
        let Statement::Assignment {
            comp,
            value: value @ Expression::If { .. },
            span,
        } = statement
        else {
            return self.walk_statement(statement);
        };
        // An element write keeps its if-expression: the statement stays one
        // assignment, so a loop that defines every element once is still a
        // total array definition (one typed Map) rather than an ordered fold.
        if is_independent_element_write(comp, value) {
            return self.walk_statement(statement);
        }
        ordered_value(
            value,
            *span,
            &mut |value| Statement::Assignment {
                comp: comp.clone(),
                value: value.clone(),
                span: *span,
            },
            &mut selected_statement,
        )
    }
}

/// An element write whose value never reads the array it writes: it defines
/// its cell from other values alone, so no ordered read of a partial
/// definition depends on the statement form.
fn is_independent_element_write(
    comp: &rumoca_core::ComponentReference,
    value: &Expression,
) -> bool {
    let [part] = comp.parts() else {
        return false;
    };
    if part.subs.is_empty() {
        return false;
    }
    let target = VarName::new(&part.ident);
    let mut references = Vec::new();
    value.collect_var_refs(&mut references);
    !references.contains(&target)
}

fn selected_statement(
    condition: &Expression,
    taken: Statement,
    fallback: Statement,
    span: Span,
) -> Statement {
    Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: condition.clone(),
            stmts: vec![taken],
        }],
        else_block: Some(vec![fallback]),
        span,
    }
}

pub(super) fn selected_capture(
    target: &VarName,
    value: &Expression,
    span: Span,
) -> Option<Box<SelectedBooleanCapture>> {
    matches!(value, Expression::If { .. }).then(|| {
        Box::new(ordered_value(
            value,
            span,
            &mut |value| SelectedBooleanCapture {
                source: Statement::Empty { span },
                plan: FunctionStatementPlan::GeneratedBooleanAssignment {
                    mutable: false,
                    target: target.clone(),
                    value: value.clone(),
                    span,
                    proven_reads: Vec::new(),
                    selected: None,
                },
            },
            &mut |condition,
                  taken: SelectedBooleanCapture,
                  fallback: SelectedBooleanCapture,
                  owner| SelectedBooleanCapture {
                source: selected_statement(condition, taken.source, fallback.source, owner),
                plan: FunctionStatementPlan::If {
                    branches: vec![vec![taken.plan]],
                    fallback: Some(vec![fallback.plan]),
                    condition_reads: vec![Vec::new()],
                    targets: Vec::new(),
                },
            },
        ))
    })
}

fn ordered_value<T>(
    value: &Expression,
    span: Span,
    leaf: &mut impl FnMut(&Expression) -> T,
    branch: &mut impl FnMut(&Expression, T, T, Span) -> T,
) -> T {
    match value {
        Expression::If {
            branches,
            else_branch,
            ..
        } => ordered_branches(branches, else_branch, span, span, leaf, branch),
        _ => leaf(value),
    }
}

fn ordered_branches<T>(
    branches: &[(Expression, Expression)],
    fallback: &Expression,
    assignment_span: Span,
    owner_span: Span,
    leaf: &mut impl FnMut(&Expression) -> T,
    branch: &mut impl FnMut(&Expression, T, T, Span) -> T,
) -> T {
    let Some(((condition, value), remaining)) = branches.split_first() else {
        return ordered_value(fallback, assignment_span, leaf, branch);
    };
    let taken = ordered_value(value, assignment_span, leaf, branch);
    let next_span = remaining
        .first()
        .and_then(|(condition, _)| condition.span())
        .unwrap_or(assignment_span);
    let continuation = ordered_branches(
        remaining,
        fallback,
        assignment_span,
        next_span,
        leaf,
        branch,
    );
    branch(condition, taken, continuation, owner_span)
}
