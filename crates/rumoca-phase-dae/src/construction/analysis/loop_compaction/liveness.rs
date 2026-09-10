//! Liveness of function-local values over the function statement tree, and the
//! store-deletion evidence built from it.
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
//!   placed after the loop. `live_in_for` and `live_in_while` join the two
//!   edges by union at the loop head for that reason, and
//!   `live_in_alternatives` names that join for the differential measurements.
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
//! Only one statement form kills a value: an assignment whose target is a whole
//! unsubscripted scalar, recognized by [`scalar_assignment_target`], which is
//! the same rule the substitution site uses to decide what it may delete. Every
//! other write, including a call's receiving element and a `reinit` target, is
//! left transparent. The answer is therefore an over-approximation of true
//! liveness wherever a value is definitely written by a form this rule does not
//! recognize, and an over-approximation is what a deletion proof needs.
//!
//! Two statement forms transfer control somewhere this module does not model:
//! `return` leaves the function and `break` leaves the enclosing loop. Both are
//! treated as transparent, which over-approximates liveness on the path that
//! falls through and can under-approximate it on the jump path.
//! [`prove_unobserved_stores`] and [`prove_unobserved_loop_local_store`]
//! therefore refuse to certify anything about a region containing either, and
//! [`contains_unstructured_jump`] is the test they use.

use super::*;
use std::collections::BTreeSet;

/// Names whose incoming value may still be observed.
pub(super) type LiveSet = BTreeSet<VarName>;

/// Evidence that no execution observes the value a store leaves in one name.
///
/// The field and the constructor are private, so a value of this type exists
/// only where [`prove_unobserved_stores`] discharged the obligations it states.
/// A caller holding no evidence for a name has no way to obtain the permission
/// the substitution site requires, which is why the site has no path that
/// deletes a store without it.
///
/// # Theorem (deleting a witnessed store preserves meaning)
///
/// Let `name` be a function-local value, `block` a list of assignments the pass
/// may rewrite, and `after` the segments that can execute once `block`
/// finishes. Suppose `prove_unobserved_stores` returns evidence for `name` at
/// `block` and `after`. Let `store` be an assignment `name := e` in `block`
/// whose target is a whole unsubscripted scalar, and let `block'` be `block`
/// with `store` deleted and `e` substituted for every later unsubscripted read
/// of `name` in `block`. Assume the three value-reproduction side conditions
/// the site establishes for `store`: no statement after `store` writes a name
/// `e` reads, `e` does not read `name`, and no statement after `store` in
/// `block` writes part of `name`. Assume also that `e` is pure (MLS §12.3):
/// where `block` holds no later unsubscripted read of `name`, `block'` drops
/// `e` instead of reproducing it, so an effect inside `e` would be lost.
/// Then, from any initial environment, if either of `block ++ after` and
/// `block' ++ after` terminates so does the other, they terminate on
/// environments that agree on every name other than `name`, and no execution of
/// `after` reads `name` before redefining it. The two interpretations therefore
/// agree on every observable outcome.
///
/// Termination is carried as a hypothesis rather than claimed, because `after`
/// may contain a `while` whose guard this module never evaluates. The rewrite
/// changes no guard and no value any guard reads, so neither program can
/// diverge where the other halts.
///
/// The evidence discharges the clause about `after`, and with it the clause the
/// syntactic predicates got wrong: `after` is entered through the operators
/// above, so a loop's back edge and its exit edge remain alternatives and
/// neither can hide the other's read. At the `for` call site the first segment
/// of `after` is the loop that encloses `block`, so `block ++ after` is an
/// unrolling of the program rather than the program itself. That is the shape
/// the conclusion needs: the segments name every point control reaches once
/// `block` finishes, however many times the enclosing loop runs.
pub(super) struct StoreUnobserved {
    name: VarName,
}

impl StoreUnobserved {
    /// The name this evidence is about.
    pub(super) fn name(&self) -> &VarName {
        &self.name
    }
}

/// Prove, for each of `names`, that nothing observes the value stored in it,
/// given the block the substitution rewrites and the segments that run after
/// it. A name absent from the result has no evidence and keeps every store.
///
/// Three obligations are established, all positively:
///
/// * every path out of `block` and out of `after` is one this module models,
///   so the dataflow answer describes the program's real successors;
/// * every read of the name inside `block` is a plain unsubscripted read,
///   which is what the substitution replaces with the stored expression; and
/// * no execution of `after` reads the name before redefining it.
///
/// The dataflow answer over `after` is computed once and then queried, so a
/// block declaring many locals costs one backward walk rather than one per
/// name.
pub(super) fn prove_unobserved_stores(
    names: &HashSet<VarName>,
    block: &[rumoca_core::Statement],
    after: &[&[rumoca_core::Statement]],
) -> HashMap<VarName, StoreUnobserved> {
    let control_flow_is_modeled = !contains_unstructured_jump(block)
        && !after
            .iter()
            .any(|segment| contains_unstructured_jump(segment));
    if !control_flow_is_modeled {
        return HashMap::new();
    }
    let observed_by_a_successor = live_in_concatenation(after, &LiveSet::new());
    names
        .iter()
        .filter(|name| {
            let no_successor_observes_it = !observed_by_a_successor.contains(*name);
            let every_read_is_substitutable = !statements_read_nonrewritable_name(block, name);
            no_successor_observes_it && every_read_is_substitutable
        })
        .map(|name| (name.clone(), StoreUnobserved { name: name.clone() }))
        .collect()
}

/// The regions a store inside a loop body can be observed from.
///
/// Every field is one of the store's successors, and the prover joins them the
/// way control flow joins them. Naming them separately is what keeps the join
/// right: `exit` is reached by falling out of the loop and `body_prefix` by
/// going around its back edge, so the two are *alternatives* and neither may be
/// allowed to hide a read in the other.
pub(super) struct LoopLocalStoreRegions<'a> {
    /// The segments whose reads of the name the rewrite replaces with the
    /// stored expression, in the order they run.
    pub(super) substituted: &'a [&'a [rumoca_core::Statement]],
    /// The statements of this loop's own body that run before the store.
    ///
    /// The back edge re-enters the body at its first statement, and the store
    /// is re-executed when control reaches it again, so these statements are
    /// exactly the ones that can observe what the previous iteration stored.
    pub(super) body_prefix: &'a [rumoca_core::Statement],
    /// The bodies of the enclosing loops, each re-entered on its own back edge.
    ///
    /// An enclosing loop re-enters after this loop has been left, without
    /// re-running this body's prefix, so each body is a path of its own rather
    /// than a continuation of the previous one.
    pub(super) enclosing_bodies: &'a [&'a [rumoca_core::Statement]],
    /// What runs once this loop is left, through to the end of the function.
    pub(super) exit: &'a [rumoca_core::Statement],
    /// The names the function's caller observes once the body returns.
    pub(super) live_on_exit: &'a LiveSet,
}

/// Evidence that no execution observes the value a store inside a loop body
/// leaves in one name.
///
/// The field and the constructor are private, so a value of this type exists
/// only where [`prove_unobserved_loop_local_store`] discharged the obligations
/// it states. The two loop-local substitution sites have no path that drops a
/// definition without one.
///
/// # Theorem (folding a witnessed definition forward preserves meaning)
///
/// Let `store` be an assignment `name := e` at some position in the body of a
/// `for` loop, with `body_prefix` the body statements before it, `substituted`
/// the segments the rewrite may edit, `enclosing_bodies` the bodies of the
/// loops enclosing this one, `exit` everything that runs once this loop is
/// left, and `live_on_exit` the names the caller observes. Suppose
/// `prove_unobserved_loop_local_store` returns evidence for `name`. Let the
/// rewrite delete `store` and substitute `e` for every unsubscripted read of
/// `name` in `substituted`.
///
/// Assume the value-reproduction side conditions the sites establish: `e` does
/// not read `name`; no statement of `substituted` that can run before a
/// substituted read writes `name`; no statement of `substituted` writes a name
/// `e` reads; and `e` is pure (MLS §12.3), because a segment holding no read of
/// `name` drops `e` instead of reproducing it. Then, from any initial
/// environment, the loop and its rewrite terminate together, they agree on
/// every name other than `name` at every point outside the loop body, and no
/// execution reaching `exit`, a back edge, or the function's return observes
/// the value `store` left.
///
/// Termination is carried as a hypothesis rather than claimed: the rewrite
/// changes no guard and no domain, so neither program can diverge where the
/// other halts.
pub(super) struct LoopLocalStoreUnobserved {
    name: VarName,
}

impl LoopLocalStoreUnobserved {
    /// The name this evidence is about.
    pub(super) fn name(&self) -> &VarName {
        &self.name
    }
}

/// Prove that nothing observes the value a store inside a loop body leaves in
/// `name`, given the regions control can reach once the store has run.
///
/// Four obligations are established, all positively:
///
/// * every path out of every region is one this module models, so the dataflow
///   answer describes the program's real successors;
/// * every read of the name inside the substituted segments is a plain
///   unsubscripted read, which is what the substitution replaces with the
///   stored expression;
/// * no execution of the loop-exit path observes the value, counting the
///   caller's view of the function's outputs as an observation at the end of
///   that path; and
/// * no execution of any back edge observes the value.
///
/// The back edges are joined by [`live_in_alternatives`] rather than composed
/// in sequence. Sequencing them would let the nearest body's write settle the
/// value an enclosing body reads, and the enclosing loop re-enters without
/// re-running the nearer body, so that write never happens on the path in
/// question.
///
/// The back-edge query uses an empty exit set rather than the exit path's
/// answer, and that is enough for both directions. It loses no precision
/// because the exit obligation is discharged first, so `name` is absent from
/// the exit answer, and this module's transfer functions move each name through
/// a statement independently of the others. It loses no soundness because a
/// path that goes around a back edge runs the statements between the store and
/// the loop head first, and those statements are the head of the exit path: a
/// write there settles the value on both paths at once, and where it does not,
/// the exit obligation has already asked about everything the back-edge path
/// can still reach.
pub(super) fn prove_unobserved_loop_local_store(
    name: &VarName,
    regions: LoopLocalStoreRegions<'_>,
) -> Option<LoopLocalStoreUnobserved> {
    // Destructured field by field, with no `..` rest pattern, so a region added
    // to the store's successors stops compiling here rather than going unasked.
    let LoopLocalStoreRegions {
        substituted,
        body_prefix,
        enclosing_bodies,
        exit,
        live_on_exit,
    } = regions;
    let control_flow_is_modeled = !contains_unstructured_jump(body_prefix)
        && !contains_unstructured_jump(exit)
        && !substituted
            .iter()
            .chain(enclosing_bodies.iter())
            .any(|segment| contains_unstructured_jump(segment));
    if !control_flow_is_modeled {
        return None;
    }
    let every_read_is_substitutable = !substituted
        .iter()
        .any(|segment| statements_read_nonrewritable_name(segment, name));
    if !every_read_is_substitutable {
        return None;
    }
    if live_in(exit, live_on_exit).contains(name) {
        return None;
    }
    let mut back_edges = Vec::with_capacity(enclosing_bodies.len() + 1);
    back_edges.push(body_prefix);
    back_edges.extend_from_slice(enclosing_bodies);
    if live_in_alternatives(&back_edges, &LiveSet::new()).contains(name) {
        return None;
    }
    Some(LoopLocalStoreUnobserved { name: name.clone() })
}

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
///
/// `live_in_for` and `live_in_while` perform this join inline at the loop head.
/// [`prove_unobserved_loop_local_store`] performs it over the back edges a
/// store inside a loop body can be re-entered by, and naming it separately also
/// lets the differential measurements compare the join against the sequence
/// composition and against execution over the corpus.
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
#[cfg(test)]
pub(super) fn reads_incoming_value(statements: &[rumoca_core::Statement], name: &VarName) -> bool {
    live_in(statements, &LiveSet::new()).contains(name)
}

/// Whether any statement transfers control by a path this module leaves
/// unmodeled.
pub(super) fn contains_unstructured_jump(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(statement_contains_unstructured_jump)
}

/// Every variant is destructured field by field, with no `..` rest pattern, so
/// a statement form that grows a field carrying nested statements stops
/// compiling here rather than hiding a jump inside it.
fn statement_contains_unstructured_jump(statement: &rumoca_core::Statement) -> bool {
    match statement {
        rumoca_core::Statement::Return { span: _ } | rumoca_core::Statement::Break { span: _ } => {
            true
        }
        rumoca_core::Statement::Empty { span: _ }
        | rumoca_core::Statement::Assignment {
            comp: _,
            value: _,
            span: _,
        }
        | rumoca_core::Statement::FunctionCall {
            comp: _,
            args: _,
            outputs: _,
            span: _,
        }
        | rumoca_core::Statement::Reinit {
            variable: _,
            value: _,
            span: _,
        }
        | rumoca_core::Statement::Assert {
            condition: _,
            message: _,
            level: _,
            span: _,
        } => false,
        rumoca_core::Statement::For {
            indices: _,
            equations,
            span: _,
        } => contains_unstructured_jump(equations),
        rumoca_core::Statement::While { block, span: _ } => {
            contains_unstructured_jump(&block.stmts)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span: _,
        } => {
            cond_blocks
                .iter()
                .any(|block| contains_unstructured_jump(&block.stmts))
                || else_block
                    .as_deref()
                    .is_some_and(contains_unstructured_jump)
        }
        rumoca_core::Statement::When { blocks, span: _ } => blocks
            .iter()
            .any(|block| contains_unstructured_jump(&block.stmts)),
    }
}

/// The transfer function of one statement.
///
/// Every variant is destructured field by field, with no `..` rest pattern, so
/// a statement form that grows a field carrying an expression stops compiling
/// here. A read position the generic statement walk reaches but this match does
/// not is a value reported dead while the program still observes it, which is
/// the one direction a deletion proof may not be wrong in. Enumerating read
/// positions by hand is exactly where such a position is easy to lose, so the
/// compiler is made to check the enumeration.
fn live_in_statement(statement: &rumoca_core::Statement, live_out: &LiveSet) -> LiveSet {
    match statement {
        rumoca_core::Statement::Empty { span: _ }
        | rumoca_core::Statement::Return { span: _ }
        | rumoca_core::Statement::Break { span: _ } => live_out.clone(),
        rumoca_core::Statement::Assignment {
            comp,
            value,
            span: _,
        } => {
            let mut live = live_out.clone();
            if let Some(target) = scalar_assignment_target(comp) {
                kill_value(&mut live, &target);
            }
            collect_expression_reads(value, &mut live);
            collect_subscript_reads(comp, &mut live);
            live
        }
        rumoca_core::Statement::Reinit {
            variable,
            value,
            span: _,
        } => {
            let mut live = live_out.clone();
            collect_expression_reads(value, &mut live);
            collect_subscript_reads(variable, &mut live);
            live
        }
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span: _,
        } => {
            let mut live = live_out.clone();
            // The callable is selected by a component reference whose
            // subscripts are ordinary value reads, evaluated where the call
            // runs. The reference's own name denotes a function rather than a
            // value, so it is not itself a read.
            if let Some(callable) = comp.component_ref() {
                collect_subscript_reads(callable, &mut live);
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
            span: _,
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
            span: _,
        } => live_in_branches(cond_blocks, else_block.as_deref(), live_out),
        // A `when` branch runs only when its condition becomes true, so the
        // edge that runs no branch at all is always available.
        rumoca_core::Statement::When { blocks, span: _ } => {
            live_in_branches(blocks, None, live_out)
        }
        rumoca_core::Statement::For {
            indices,
            equations,
            span: _,
        } => live_in_for(indices, equations, live_out),
        rumoca_core::Statement::While { block, span: _ } => live_in_while(block, live_out),
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

/// Remove `name` and every value nested beneath it.
///
/// Defining `r` in full defines `r.field`; defining `r.field` leaves `r`'s
/// other fields observable, which is why the enclosing names survive.
fn kill_value(live: &mut LiveSet, name: &VarName) {
    let nested = format!("{}.", name.as_str());
    live.retain(|candidate| candidate != name && !candidate.as_str().starts_with(&nested));
}

/// The reads carried by the subscripts of a component reference.
///
/// The subscript forms are matched field by field for the same reason the
/// statement forms are: a form that grows an expression field has to stop
/// compiling here rather than drop the read it carries.
pub(super) fn collect_subscript_reads(
    component: &rumoca_core::ComponentReference,
    reads: &mut LiveSet,
) {
    for part in component.parts() {
        for subscript in &part.subs {
            match subscript {
                Subscript::Expr { expr, span: _ } => collect_expression_reads(expr, reads),
                // A literal index and a whole-dimension colon name no value.
                Subscript::Index { value: _, span: _ } | Subscript::Colon { span: _ } => {}
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
