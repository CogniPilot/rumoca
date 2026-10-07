//! MLS 3.7 §11.2.3 `while` loops with a proven iteration bound, lowered to the
//! compact `for` owner.
//!
//! `while c loop S end while` runs `S` until `c` first evaluates false. Once
//! `c` is false the loop changes no value, so `c` stays false; hence, for any
//! `B` at least the number of iterations the loop can run,
//!
//! ```text
//! for w in 1:B loop if c then S end if; end for;
//! ```
//!
//! computes the same values. Its extra evaluations of `c` all happen in the
//! state the `while` loop's final (false) test already evaluated `c` in, so
//! they raise no error the original does not.
//!
//! The bound is proven from the value facts (`guard_facts`) at the loop, in
//! one of two forms. The counter form:
//!
//! * a top-level conjunct of `c` is `k < N`, `k <= N`, `N > k`, or `N >= k`,
//!   with `N` settled at translation (a literal, a shape extent, an evaluable
//!   parameter) and not written by `S`;
//! * `S` has no `break` or `return`, writes `k` once at top level as
//!   `k := k + d` with a positive Integer literal `d`, writes `k` after that
//!   statement nowhere, and before it only as `k := N` (the `isEqual` early
//!   exit), which cannot lower `k` while `c` holds;
//! * `k` has a proven floor `s` whenever `c` is evaluated: either `c` reads an
//!   array element with `k` as a subscript (MLS §10.5 makes an index below 1
//!   an error, so `s = 1`), or the facts on entry bound `k` below by `s` (its
//!   last dominating assignment, or its declaration binding when nothing
//!   wrote it), as the Media inversions' `Integer i = 0` counters do.
//!
//! Then every iteration raises `k` by at least 1 from at least `s` while `k`
//! stays at most `N`, so `B = N - min(s, 1) + 1` iterations suffice (`B = N`
//! for `s >= 1`). The progress form (`progress`) proves that every pass
//! either ends the loop or raises a counter by at least 1 from within a
//! proven interval. A loop without either proof is left as written and keeps
//! its typed rejection.

use super::super::guard_facts::{FactScope, GuardFacts};
use super::*;
use rumoca_core::{ForIndex, Literal, OpBinary, StatementBlock};

mod progress;
use progress::progress_bound;

/// `statements` with every `while` loop they contain bounded, reading the
/// facts that hold before them.
pub(super) fn bound_while_loops(
    statements: &[rumoca_core::Statement],
    scope: FactScope<'_>,
    mut facts: GuardFacts,
) -> Vec<rumoca_core::Statement> {
    let mut bounded = Vec::with_capacity(statements.len());
    for statement in statements {
        bounded.extend(bound_statement(statement, scope, &facts));
        facts.after(statement, scope);
    }
    bounded
}

/// `statement` with every `while` loop it contains bounded.
///
/// A bounded loop whose condition provably holds on entry runs its body at
/// least once, so that first iteration is written out unconditionally ahead
/// of the guarded remainder; the definitions it makes are then visible on
/// every path, as the source loop guarantees.
fn bound_statement(
    statement: &rumoca_core::Statement,
    scope: FactScope<'_>,
    facts: &GuardFacts,
) -> Vec<rumoca_core::Statement> {
    let rumoca_core::Statement::While { block, span } = statement else {
        return vec![bound_nested_statement(statement, scope, facts)];
    };
    let head = facts.while_head(block, scope);
    let mut pass = head.clone();
    pass.observe_accesses(&block.cond, scope);
    let pass = pass.assuming(&block.cond, true, scope);
    let body = bound_while_loops(&block.stmts, scope, pass);
    let block = StatementBlock {
        cond: block.cond.clone(),
        stmts: body,
    };
    let Some(bound) = iteration_bound(&block, facts, &head, scope) else {
        return vec![rumoca_core::Statement::While { block, span: *span }];
    };
    if !facts.assuming(&block.cond, false, scope).is_unreachable() {
        return vec![guarded_for(block, bound, *span)];
    }
    let mut peeled = block.stmts.clone();
    if bound > 1 {
        peeled.push(guarded_for(block, bound - 1, *span));
    }
    peeled
}

fn bound_nested_statement(
    statement: &rumoca_core::Statement,
    scope: FactScope<'_>,
    facts: &GuardFacts,
) -> rumoca_core::Statement {
    match statement {
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } => {
            let binders = indices
                .iter()
                .map(|index| VarName::new(&index.ident))
                .collect::<Vec<_>>();
            let body = facts
                .for_head(indices, equations, scope)
                .loop_entry(&[], &binders);
            rumoca_core::Statement::For {
                indices: indices.clone(),
                equations: bound_while_loops(equations, scope, body),
                span: *span,
            }
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => {
            let conditions = cond_blocks
                .iter()
                .map(|block| &block.cond)
                .collect::<Vec<_>>();
            let mut entries = facts.branch_entries(&conditions, scope);
            let fallthrough = entries.pop();
            rumoca_core::Statement::If {
                cond_blocks: cond_blocks
                    .iter()
                    .zip(entries)
                    .map(|(block, entry)| StatementBlock {
                        cond: block.cond.clone(),
                        stmts: bound_while_loops(&block.stmts, scope, entry),
                    })
                    .collect(),
                else_block: else_block
                    .as_ref()
                    .zip(fallthrough)
                    .map(|(statements, entry)| bound_while_loops(statements, scope, entry)),
                span: *span,
            }
        }
        _ => statement.clone(),
    }
}

fn guarded_for(block: StatementBlock, bound: i64, span: Span) -> rumoca_core::Statement {
    let literal = |value| Expression::Literal {
        value: Literal::Integer(value),
        span,
    };
    rumoca_core::Statement::For {
        indices: vec![ForIndex {
            ident: format!("__rumoca_while_{}", span.start.0),
            range: Expression::Range {
                start: Box::new(literal(1)),
                step: None,
                end: Box::new(literal(bound)),
                span,
            },
        }],
        equations: vec![rumoca_core::Statement::If {
            cond_blocks: vec![block],
            else_block: None,
            span,
        }],
        span,
    }
}

/// The proven iteration bound of one `while` loop (see the module note), from
/// the facts before the loop and at its head.
fn iteration_bound(
    block: &StatementBlock,
    entry: &GuardFacts,
    head: &GuardFacts,
    scope: FactScope<'_>,
) -> Option<i64> {
    if statements_exit_early(&block.stmts) {
        return None;
    }
    counter_bound(block, entry, scope).or_else(|| progress_bound(block, head, scope))
}

/// The counter proof form (see the module note).
fn counter_bound(block: &StatementBlock, entry: &GuardFacts, scope: FactScope<'_>) -> Option<i64> {
    let mut conjuncts = Vec::new();
    collect_conjuncts(&block.cond, &mut conjuncts);
    conjuncts.iter().find_map(|conjunct| {
        let (counter, limit) = counter_limit(conjunct)?;
        let bound = scope.shapes.proven_extent(limit)?;
        let written = statements_written_names(&block.stmts);
        let limit_invariant = {
            let mut reads = Vec::new();
            limit.collect_var_refs(&mut reads);
            reads.iter().all(|name| !written.contains(name))
        };
        // The least value `k` can hold when `c` is evaluated: 1 when `c`
        // indexes an array with it, otherwise its least value on entry.
        let floor = if indexes_with(&block.cond, counter) {
            Some(1)
        } else {
            entry.integer_interval(&reference(counter), scope).lower
        }?;
        let iterations = iterations_from(bound, floor)?;
        (bound >= 0
            && limit_invariant
            && advances_each_iteration(&block.stmts, counter, bound, scope.shapes))
        .then_some(iterations)
    })
}

/// A plain reference to the scalar `name`.
fn reference(name: &VarName) -> Expression {
    Expression::VarRef {
        name: Reference::generated(name.as_str()),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

/// An iteration bound for a counter that rises by at least 1 per iteration
/// from at least `floor` while it stays at most `limit`: `limit - floor + 1`.
/// A floor above 1 is taken as 1, which keeps the bound `limit` and only
/// overestimates the iterations.
fn iterations_from(limit: i64, floor: i64) -> Option<i64> {
    let floor = floor.min(1);
    limit.checked_sub(floor)?.checked_add(1)
}

fn collect_conjuncts<'a>(expression: &'a Expression, conjuncts: &mut Vec<&'a Expression>) {
    if let Expression::Binary {
        op: OpBinary::And,
        lhs,
        rhs,
        ..
    } = expression
    {
        collect_conjuncts(lhs, conjuncts);
        collect_conjuncts(rhs, conjuncts);
    } else {
        conjuncts.push(expression);
    }
}

/// `k < N`, `k <= N`, `N > k`, or `N >= k` for a plain reference `k`.
fn counter_limit(expression: &Expression) -> Option<(&VarName, &Expression)> {
    let Expression::Binary { op, lhs, rhs, .. } = expression else {
        return None;
    };
    let (counter, limit) = match op {
        OpBinary::Lt | OpBinary::Le => (lhs, rhs),
        OpBinary::Gt | OpBinary::Ge => (rhs, lhs),
        _ => return None,
    };
    plain_reference(counter).map(|name| (name, limit.as_ref()))
}

fn plain_reference(expression: &Expression) -> Option<&VarName> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.var_name()),
        _ => None,
    }
}

/// The body raises `counter` by a positive literal once per iteration: one
/// top-level `k := k + d`, no write of `k` after it, and before it only
/// `k := N` (the limit itself), anywhere.
fn advances_each_iteration(
    statements: &[rumoca_core::Statement],
    counter: &VarName,
    limit: i64,
    shapes: &ShapeEnvironment,
) -> bool {
    let Some(increment) = statements.iter().position(|statement| {
        matches!(statement, rumoca_core::Statement::Assignment { comp, value, .. }
            if comp.parts().iter().all(|part| part.subs.is_empty())
                && rumoca_core::component_ref_to_base_reference(comp).var_name() == counter
                && is_positive_increment(value, counter))
    }) else {
        return false;
    };
    !statements_written_names(&statements[increment + 1..]).contains(counter)
        && writes_only_limit(&statements[..increment], counter, limit, shapes)
}

/// Every write of `counter` in `statements` assigns exactly `limit`.
fn writes_only_limit(
    statements: &[rumoca_core::Statement],
    counter: &VarName,
    limit: i64,
    shapes: &ShapeEnvironment,
) -> bool {
    statements.iter().all(|statement| match statement {
        rumoca_core::Statement::Assignment { comp, value, .. }
            if rumoca_core::component_ref_to_base_reference(comp).var_name() == counter =>
        {
            comp.parts().iter().all(|part| part.subs.is_empty())
                && shapes.proven_extent(value) == Some(limit)
        }
        rumoca_core::Statement::For { equations, .. } => {
            writes_only_limit(equations, counter, limit, shapes)
        }
        rumoca_core::Statement::While { block, .. } => {
            writes_only_limit(&block.stmts, counter, limit, shapes)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .all(|block| writes_only_limit(&block.stmts, counter, limit, shapes))
                && else_block
                    .as_ref()
                    .is_none_or(|statements| writes_only_limit(statements, counter, limit, shapes))
        }
        other => !statements_written_names(std::slice::from_ref(other)).contains(counter),
    })
}

fn is_positive_increment(value: &Expression, counter: &VarName) -> bool {
    let Expression::Binary {
        op: OpBinary::Add,
        lhs,
        rhs,
        ..
    } = value
    else {
        return false;
    };
    let positive = |expression: &Expression| {
        matches!(
            expression,
            Expression::Literal {
                value: Literal::Integer(step),
                ..
            } if *step > 0
        )
    };
    (plain_reference(lhs) == Some(counter) && positive(rhs))
        || (plain_reference(rhs) == Some(counter) && positive(lhs))
}

/// Whether `expression` subscripts an array with exactly `counter` (MLS §10.5:
/// an index below 1 is an error, so every error-free evaluation has `k >= 1`).
fn indexes_with(expression: &Expression, counter: &VarName) -> bool {
    expression.contains_subexpression(|node| {
        let subscripts = match node {
            Expression::VarRef { subscripts, .. } | Expression::Index { subscripts, .. } => {
                subscripts.as_slice()
            }
            _ => &[],
        };
        subscripts.iter().any(|subscript| {
            matches!(subscript, Subscript::Expr { expr, .. }
                if plain_reference(expr) == Some(counter))
        })
    })
}

fn statements_exit_early(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Break { .. } | rumoca_core::Statement::Return { .. } => true,
        rumoca_core::Statement::For { equations, .. } => statements_exit_early(equations),
        rumoca_core::Statement::While { block, .. } => statements_exit_early(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_exit_early(&block.stmts))
                || else_block
                    .as_ref()
                    .is_some_and(|statements| statements_exit_early(statements))
        }
        _ => false,
    })
}

/// Every name an assignment, call output, or loop index in `statements`
/// writes.
fn statements_written_names(statements: &[rumoca_core::Statement]) -> HashSet<VarName> {
    let mut written = HashSet::new();
    collect_written_names(statements, &mut written);
    written
}

fn collect_written_names(statements: &[rumoca_core::Statement], written: &mut HashSet<VarName>) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } => {
                written.insert(
                    rumoca_core::component_ref_to_base_reference(comp)
                        .var_name()
                        .clone(),
                );
            }
            rumoca_core::Statement::FunctionCall { outputs, .. } => {
                written.extend(outputs.iter().flatten().map(|output| {
                    rumoca_core::component_ref_to_base_reference(output)
                        .var_name()
                        .clone()
                }));
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                written.extend(indices.iter().map(|index| VarName::new(&index.ident)));
                collect_written_names(equations, written);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_written_names(&block.stmts, written);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_written_names(&block.stmts, written);
                }
                if let Some(statements) = else_block {
                    collect_written_names(statements, written);
                }
            }
            _ => {}
        }
    }
}
