//! Liveness of function-local values over the function statement tree.
//!
//! The store-deletion proofs in this module ask one question in many syntactic
//! disguises: is the value a statement writes ever observed again? This is the
//! dataflow answer to that question, stated once.
//!
//! A name is live at a program point when some path from that point reads its
//! current value before overwriting it. `live_in` is computed backwards from a
//! set of names live on exit, so `live_in(body, {})` is exactly "the value
//! flowing into `body` is read somewhere inside it".
//!
//! Control flow is modeled by which successors compose and how:
//!
//! * A statement sequence composes its successors in order: the transfer
//!   function of `[s0, s1]` is the transfer of `s0` applied to the result of
//!   `s1`, so a definite write in `s0` hides a read in `s1`.
//!   [`live_in_concatenation`] is the operator for that shape.
//! * A loop's back edge and its exit edge are alternative successors of the
//!   body's last statement, never a sequence. A name is live there when either
//!   successor observes it, so the body's own write can never hide a read
//!   placed after the loop. [`live_in_alternatives`] is the operator for that
//!   shape.
//! * A `for` domain and a `while` guard may admit zero iterations (MLS
//!   §11.2.2), so the loop-exit edge leaves the loop head: whatever is live
//!   after the loop is live before it, whatever the body writes.
//! * An `if` without an `else`, and every `when`, have a fall-through edge that
//!   runs no branch at all, so they cannot kill anything.
//!
//! Loop bodies are solved by ascending iteration from the empty set, which
//! reaches the least fixed point because every transfer function here is
//! monotone in its exit set.
//!
//! A `for` binder shadows an enclosing name of the same text inside the body
//! (MLS §11.2.2), so a binder is removed from the body's answer before it
//! reaches the enclosing scope.
//!
//! Two statement forms transfer control somewhere this module does not model:
//! `return` leaves the function and `break` leaves the enclosing loop. Both are
//! treated as transparent, which over-approximates liveness on the path that
//! falls through and can under-approximate it on the jump path. Ask
//! [`contains_unstructured_jump`] before trusting an answer about a body that
//! contains either.

use super::*;
use std::collections::BTreeSet;

/// Names whose incoming value may still be observed.
pub(super) type LiveSet = BTreeSet<VarName>;

/// Names live on entry to `statements`, given the names live on exit.
pub(super) fn live_in(statements: &[rumoca_core::Statement], live_out: &LiveSet) -> LiveSet {
    let mut live = live_out.clone();
    for statement in statements.iter().rev() {
        live = live_in_statement(statement, &live);
    }
    live
}

/// Names live on entry to `segments` executed one after another.
///
/// Use this when each segment genuinely falls through into the next: the rest
/// of a block followed by the rest of its enclosing block, for instance.
pub(super) fn live_in_concatenation(
    segments: &[&[rumoca_core::Statement]],
    live_out: &LiveSet,
) -> LiveSet {
    let mut live = live_out.clone();
    for segment in segments.iter().rev() {
        live = live_in(segment, &live);
    }
    live
}

/// Names live on entry to any of `segments`, each entered independently.
///
/// Use this when the segments are competing successors of one point, such as
/// the several loop bodies a definition can be re-entered by. Composing them in
/// sequence instead would let a write in one segment hide a read in another,
/// which is the mistake this operator exists to prevent. With no segments there
/// is no path, so nothing is live.
pub(super) fn live_in_alternatives(
    segments: &[&[rumoca_core::Statement]],
    live_out: &LiveSet,
) -> LiveSet {
    let mut live = LiveSet::new();
    for segment in segments {
        live.extend(live_in(segment, live_out));
    }
    live
}

/// Whether the value `name` holds on entry to `statements` is read inside them.
pub(super) fn reads_incoming_value(statements: &[rumoca_core::Statement], name: &VarName) -> bool {
    live_in(statements, &LiveSet::new()).contains(name)
}

/// Whether any statement transfers control by a path this module leaves
/// unmodeled.
pub(super) fn contains_unstructured_jump(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Return { .. } | rumoca_core::Statement::Break { .. } => true,
        rumoca_core::Statement::For { equations, .. } => contains_unstructured_jump(equations),
        rumoca_core::Statement::While { block, .. } => contains_unstructured_jump(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| contains_unstructured_jump(&block.stmts))
                || else_block
                    .as_deref()
                    .is_some_and(contains_unstructured_jump)
        }
        rumoca_core::Statement::When { blocks, .. } => blocks
            .iter()
            .any(|block| contains_unstructured_jump(&block.stmts)),
        _ => false,
    })
}

fn live_in_statement(statement: &rumoca_core::Statement, live_out: &LiveSet) -> LiveSet {
    match statement {
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => live_out.clone(),
        rumoca_core::Statement::Assignment { comp, value, .. }
        | rumoca_core::Statement::Reinit {
            variable: comp,
            value,
            ..
        } => {
            let mut live = live_out.clone();
            if let Some(target) = whole_definition_target(comp) {
                kill_value(&mut live, &target);
            }
            collect_expression_reads(value, &mut live);
            collect_subscript_reads(comp, &mut live);
            live
        }
        rumoca_core::Statement::FunctionCall { args, outputs, .. } => {
            let mut live = live_out.clone();
            for output in outputs.iter().flatten() {
                if let Some(target) = whole_definition_target(output) {
                    kill_value(&mut live, &target);
                }
            }
            for argument in args {
                collect_expression_reads(argument, &mut live);
            }
            for output in outputs.iter().flatten() {
                collect_subscript_reads(output, &mut live);
            }
            live
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            let mut live = live_out.clone();
            collect_expression_reads(condition, &mut live);
            collect_expression_reads(message, &mut live);
            if let Some(level) = level.as_deref() {
                collect_expression_reads(level, &mut live);
            }
            live
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => live_in_branches(cond_blocks, else_block.as_deref(), live_out),
        // A `when` branch runs only when its condition becomes true, so the
        // edge that runs no branch at all is always available.
        rumoca_core::Statement::When { blocks, .. } => live_in_branches(blocks, None, live_out),
        rumoca_core::Statement::For {
            indices, equations, ..
        } => live_in_for(indices, equations, live_out),
        rumoca_core::Statement::While { block, .. } => live_in_while(block, live_out),
    }
}

fn live_in_branches(
    branches: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    live_out: &LiveSet,
) -> LiveSet {
    // Without a fallback branch the statement has an edge that runs no branch,
    // so everything observed after it is observed on that edge too.
    let mut live = match fallback {
        Some(statements) => live_in(statements, live_out),
        None => live_out.clone(),
    };
    for branch in branches {
        live.extend(live_in(&branch.stmts, live_out));
        collect_expression_reads(&branch.cond, &mut live);
    }
    live
}

fn live_in_for(
    indices: &[rumoca_core::ForIndex],
    equations: &[rumoca_core::Statement],
    live_out: &LiveSet,
) -> LiveSet {
    let binders = indices
        .iter()
        .map(|index| VarName::new(&index.ident))
        .collect::<Vec<_>>();
    let mut carried = LiveSet::new();
    loop {
        // The body's last statement reaches the back edge or the loop exit.
        // They are alternatives, so their live sets meet by union.
        let mut body_out = carried.clone();
        body_out.extend(live_out.iter().cloned());
        let mut next = live_in(equations, &body_out);
        for binder in &binders {
            kill_value(&mut next, binder);
        }
        if next == carried {
            break;
        }
        carried = next;
    }
    let mut live = carried;
    live.extend(live_out.iter().cloned());
    // A later index range is evaluated inside the earlier binders' scope.
    for (position, index) in indices.iter().enumerate() {
        let mut reads = LiveSet::new();
        collect_expression_reads(&index.range, &mut reads);
        for earlier in &indices[..position] {
            kill_value(&mut reads, &VarName::new(&earlier.ident));
        }
        live.extend(reads);
    }
    live
}

fn live_in_while(block: &rumoca_core::StatementBlock, live_out: &LiveSet) -> LiveSet {
    // The guard is re-evaluated at the loop head, which the back edge and the
    // entry edge both reach; the head's other successor is the loop exit.
    let mut head = live_out.clone();
    loop {
        let mut next = live_in(&block.stmts, &head);
        next.extend(live_out.iter().cloned());
        collect_expression_reads(&block.cond, &mut next);
        if next == head {
            break;
        }
        head = next;
    }
    head
}

/// The name a write defines in full, when it covers the whole value.
///
/// A subscripted write leaves the other elements of the value intact, so it
/// neither kills the incoming value nor reads it.
pub(super) fn whole_definition_target(
    component: &rumoca_core::ComponentReference,
) -> Option<VarName> {
    component
        .parts()
        .iter()
        .all(|part| part.subs.is_empty())
        .then(|| component.to_var_name())
}

/// Remove `name` and every value nested beneath it.
///
/// Defining `r` in full defines `r.field`; defining `r.field` leaves `r`'s
/// other fields observable, which is why the enclosing names survive.
fn kill_value(live: &mut LiveSet, name: &VarName) {
    let nested = format!("{}.", name.as_str());
    live.retain(|candidate| candidate != name && !candidate.as_str().starts_with(&nested));
}

pub(super) fn collect_subscript_reads(
    component: &rumoca_core::ComponentReference,
    reads: &mut LiveSet,
) {
    for part in component.parts() {
        for subscript in &part.subs {
            if let Subscript::Expr { expr, .. } = subscript {
                collect_expression_reads(expr, reads);
            }
        }
    }
}

pub(super) fn collect_expression_reads(expression: &Expression, reads: &mut LiveSet) {
    struct Collector<'a> {
        reads: &'a mut LiveSet,
    }
    impl rumoca_core::ExpressionVisitor for Collector<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            insert_read(self.reads, reference.var_name());
            self.walk_var_ref(reference, subscripts);
        }
    }
    let mut collector = Collector { reads };
    rumoca_core::ExpressionVisitor::visit_expression(&mut collector, expression);
}

/// Record a read of `name` and of every value it is nested in.
///
/// Reading `r.field` observes part of `r`, so a proof about `r` must see it.
fn insert_read(reads: &mut LiveSet, name: &VarName) {
    let text = name.as_str();
    let mut depth = 0usize;
    for (offset, byte) in text.bytes().enumerate() {
        match byte {
            b'[' => depth += 1,
            b']' => depth = depth.saturating_sub(1),
            b'.' if depth == 0 => {
                reads.insert(VarName::intern(&text[..offset]));
            }
            _ => {}
        }
    }
    reads.insert(name.clone());
}
