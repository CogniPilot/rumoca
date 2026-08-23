//! Whole-array marshalling copies at a call boundary, and when they may go.
//!
//! # What the copies are
//!
//! An owner that calls a protected function stages each array actual into a
//! temporary it owns, calls the callee, and copies each array result out of the
//! callee's context region into a temporary of its own. Both temporaries are
//! ordinary slots in the caller's working memory, and each one costs a full
//! traversal of the aggregate plus its own storage.
//!
//! Two of those traversals are pure traffic:
//!
//! * **A staged actual** whose only definition is a whole-array copy of an
//!   aggregate the caller already holds. The callee reads the same elements
//!   either way, so the call may name the aggregate itself.
//! * **A result** whose only use is a whole-array copy into another aggregate.
//!   The read-back may write that aggregate directly.
//!
//! In both cases the temporary then has no use at all and leaves the region,
//! which is where the storage saving comes from: two slots the arm overlay used
//! to place are two slots it no longer has to place.
//!
//! # Why it is sound here
//!
//! The rewrite moves a *read* later (the actual is read at the call rather than
//! at the copy) or a *write* earlier (the result lands in its destination at
//! the read-back rather than after it). Both are value-preserving exactly when
//! nothing writes the aggregate in between and nothing else observes the
//! temporary, which is what [`permission`] proves before it will mint a
//! [`permission::Forwarding`].
//!
//! The aliasing question a C `restrict` would have to answer is answered
//! structurally instead, and only for objects this view can separate:
//!
//! * The forwarded aggregate is always a **local** of the calling owner: a
//!   frame object or a slot in the calling owner's own region. A callee writes
//!   its own region, the regions of its own callees, and (when it is stateful)
//!   block state. It never writes a caller's local, because no reference in the
//!   callee's body can name one: the callee's formal parameters are its inputs,
//!   and checked construction proves an input is never written. Block state and
//!   a local are distinct members of the block-state struct. A caller's region
//!   and a callee's region are distinct members too, because the overlay prover
//!   refuses to place a caller and a transitive callee in one group.
//! * No actual parameter of the emitted call is writable at all, so the
//!   "two writable actuals that alias" pair cannot arise in this protocol; the
//!   obligation is discharged by the shape of the protocol rather than by a
//!   check. What *can* arise is the case above, and a source that is not a
//!   local of the calling owner is refused rather than reasoned about.
//!
//! Everything else fails closed: an unreadable construct anywhere in the body,
//! a shape that is not literal on both sides, a temporary with any other
//! mention, an aggregate touched between the copy and the call. The copy is
//! always correct, so refusing costs traffic and nothing else.

use std::collections::{HashMap, HashSet};

use rumoca_ir_galec::ast;

use super::{
    KernelStatementView, TypedCallView, TypedConditionView, TypedExpressionNodeView,
    TypedExpressionView, TypedLimitTargetView, TypedRefPartView, TypedReferenceNodeView,
    TypedReferenceView, TypedSpannedStatement, TypedStatementView, is_iterator_reference,
    reference_of,
};

/// Drop every provably redundant marshalling copy in one owner's body and
/// report the slot names that no longer have a use.
///
/// `retirable` is the set of names a caller is willing to remove from the
/// region: the owner's own local declarations. An output parameter is never in
/// it, because the owner's caller reads it after the owner returns and no
/// analysis of this body can see that.
pub(super) fn forward<'a>(
    statements: &mut [TypedSpannedStatement<'a>],
    retirable: &HashSet<&'a str>,
) -> HashSet<&'a str> {
    let mut retired = HashSet::new();
    if retirable.is_empty() || Census::of(statements).opaque {
        return retired;
    }
    // Results first. A staged actual is very often the copy of a result, and
    // forwarding the read-back into the destination is the better of the two
    // rewrites there: it removes the same traversal and leaves a named
    // aggregate behind rather than the temporary.
    rewrite(statements, Direction::Result, retirable, &mut retired);
    rewrite(statements, Direction::Argument, retirable, &mut retired);
    retired
}

/// Which side of the call boundary a rewrite pass works on.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Direction {
    /// The read-back of a result into a temporary the caller then copies on.
    Result,
    /// The staging of an actual into a temporary the caller then passes.
    Argument,
}

/// Rewrite one direction over every statement list in the body.
///
/// The census is taken once per direction, over the whole body, so a pass sees
/// the mentions the previous pass left rather than the ones it started with.
fn rewrite<'a>(
    statements: &mut [TypedSpannedStatement<'a>],
    direction: Direction,
    retirable: &HashSet<&'a str>,
    retired: &mut HashSet<&'a str>,
) {
    let census = Census::of(statements);
    rewrite_list(statements, direction, &census, retirable, retired);
}

fn rewrite_list<'a>(
    list: &mut [TypedSpannedStatement<'a>],
    direction: Direction,
    census: &Census<'a>,
    retirable: &HashSet<&'a str>,
    retired: &mut HashSet<&'a str>,
) {
    for statement in list.iter_mut() {
        match &mut statement.node {
            TypedStatementView::If(conditional) => {
                for branch in &mut conditional.branches {
                    rewrite_list(&mut branch.body, direction, census, retirable, retired);
                }
                if let Some(body) = &mut conditional.else_body {
                    rewrite_list(body, direction, census, retirable, retired);
                }
            }
            TypedStatementView::For(loop_view) => {
                rewrite_list(&mut loop_view.body, direction, census, retirable, retired);
            }
            _ => {}
        }
    }
    apply(list, plan(list, direction, census, retirable), retired);
}

/// Every forwarding one statement list admits, with conflicting ones dropped.
///
/// A permission is evidence about the list it was minted against, and applying
/// one changes that list, so a plan that would disturb an earlier plan's
/// evidence is discarded rather than re-proved. Three things make two plans
/// disturb one another: absorbing one statement twice, absorbing the statement
/// another plan rewrites, and naming an aggregate another plan already moved.
/// One call statement may host several forwardings (a call has several result
/// slots and several actuals), so a rewritten call is not itself a conflict.
fn plan<'a>(
    list: &[TypedSpannedStatement<'a>],
    direction: Direction,
    census: &Census<'a>,
    retirable: &HashSet<&'a str>,
) -> Vec<permission::Forwarding<'a>> {
    let per_statement: Vec<Census<'a>> = list.iter().map(Census::of_statement).collect();
    let mut claimed_names: HashSet<&'a str> = HashSet::new();
    let mut absorbed: HashSet<usize> = HashSet::new();
    let mut rewritten: HashSet<usize> = HashSet::new();
    let mut plans = Vec::new();
    for call_index in 0..list.len() {
        if absorbed.contains(&call_index) {
            continue;
        }
        let minted = match direction {
            Direction::Result => {
                permission::forward_results(list, &per_statement, census, retirable, call_index)
            }
            Direction::Argument => {
                permission::forward_arguments(list, &per_statement, census, retirable, call_index)
            }
        };
        for forwarding in minted {
            let names = [forwarding.retired(), forwarding.object_root()];
            let copy_index = forwarding.copy_index();
            if names.iter().any(|name| claimed_names.contains(name))
                || absorbed.contains(&copy_index)
                || rewritten.contains(&copy_index)
            {
                continue;
            }
            claimed_names.extend(names);
            absorbed.insert(copy_index);
            rewritten.insert(call_index);
            plans.push(forwarding);
        }
    }
    plans
}

fn apply<'a>(
    list: &mut [TypedSpannedStatement<'a>],
    plans: Vec<permission::Forwarding<'a>>,
    retired: &mut HashSet<&'a str>,
) {
    for forwarding in plans {
        let copy_index = forwarding.copy_index();
        let call_index = forwarding.call_index();
        let position = forwarding.position();
        let retired_name = forwarding.retired();
        let object = forwarding.into_object();
        match forwarding_slot(emitted_mut(&mut list[call_index]), position) {
            Some(Slot::Target(target)) => *target = object,
            Some(Slot::Actual(actual)) => {
                *actual = TypedExpressionView {
                    rank: object.rank,
                    extents: object.extents.clone(),
                    scalar: object.scalar,
                    node: TypedExpressionNodeView::Ref(object),
                };
            }
            None => continue,
        }
        list[copy_index].kernel = Some(KernelStatementView::Absorbed);
        retired.insert(retired_name);
    }
}

/// The statement a C-family target prints for this entry.
///
/// A rewritten call carries its rewrite in the sibling `kernel` slot rather
/// than over the node, so the checked Algorithm Code rendering, which reads
/// only the node, still prints the call and the copy the block spells. Every
/// read this module makes about what the target will emit goes through here.
fn emitted<'view, 'a>(
    statement: &'view TypedSpannedStatement<'a>,
) -> &'view TypedStatementView<'a> {
    match &statement.kernel {
        Some(KernelStatementView::Marshalled { statement }) => statement,
        _ => &statement.node,
    }
}

/// Whether a statement is one this module may still rewrite: an untouched
/// statement, or a call it has already rewritten once. Any other kernel means
/// the emission belongs to that kernel and is not this module's to move.
fn emittable(statement: &TypedSpannedStatement<'_>) -> bool {
    matches!(
        statement.kernel,
        None | Some(KernelStatementView::Marshalled { .. })
    )
}

/// The same as [`emitted`], for writing: the first rewrite of a statement moves
/// a copy of the node into the sibling slot, and every later one edits that
/// copy, so several forwardings on one call compose.
fn emitted_mut<'view, 'a>(
    statement: &'view mut TypedSpannedStatement<'a>,
) -> &'view mut TypedStatementView<'a> {
    if !matches!(
        statement.kernel,
        Some(KernelStatementView::Marshalled { .. })
    ) {
        statement.kernel = Some(KernelStatementView::Marshalled {
            statement: Box::new(statement.node.clone()),
        });
    }
    match &mut statement.kernel {
        Some(KernelStatementView::Marshalled { statement }) => statement,
        // Unreachable: the branch above has just installed the variant.
        _ => &mut statement.node,
    }
}

/// Where a forwarded aggregate lands in the rewritten call statement.
enum Slot<'view, 'a> {
    Target(&'view mut TypedReferenceView<'a>),
    Actual(&'view mut TypedExpressionView<'a>),
}

/// The result slot at `position`, or the actual at `position` when the call has
/// no result slot there.
///
/// A call's result slots and its actuals are disjoint sequences, and a
/// [`permission::Forwarding`] carries the position it was minted for together
/// with the direction that minted it. The two never collide because a plan for
/// one direction is only ever applied by the pass that minted it.
fn forwarding_slot<'view, 'a>(
    statement: &'view mut TypedStatementView<'a>,
    position: permission::Position,
) -> Option<Slot<'view, 'a>> {
    match position {
        permission::Position::Target(index) => match statement {
            TypedStatementView::Assignment { target, .. } if index == 0 => {
                Some(Slot::Target(target))
            }
            TypedStatementView::MultiAssignment { targets, .. } => {
                targets.get_mut(index).map(Slot::Target)
            }
            _ => None,
        },
        permission::Position::Actual(index) => call_mut(statement)
            .and_then(|call| call.arguments.get_mut(index))
            .map(Slot::Actual),
    }
}

fn call_mut<'view, 'a>(
    statement: &'view mut TypedStatementView<'a>,
) -> Option<&'view mut TypedCallView<'a>> {
    match statement {
        TypedStatementView::Assignment { value, .. } => match &mut value.node {
            TypedExpressionNodeView::Call(call) => Some(call),
            _ => None,
        },
        TypedStatementView::MultiAssignment { call, .. } | TypedStatementView::Call(call) => {
            Some(call)
        }
        _ => None,
    }
}

/// The user call a statement performs, with its result slots in signature
/// order.
fn user_call<'view, 'a>(
    statement: &'view TypedStatementView<'a>,
) -> Option<(&'view TypedCallView<'a>, Vec<&'view TypedReferenceView<'a>>)> {
    let (call, targets) = match statement {
        TypedStatementView::Assignment { target, value } => match &value.node {
            TypedExpressionNodeView::Call(call) => (call, vec![target]),
            _ => return None,
        },
        TypedStatementView::MultiAssignment { targets, call } => (call, targets.iter().collect()),
        TypedStatementView::Call(call) => (call, Vec::new()),
        _ => return None,
    };
    call.user_function.then_some((call, targets))
}

/// The extents of the whole declared array a reference names, or `None` when
/// the reference is not that bare object.
///
/// Both halves are required: every part unsubscripted, so the reference is the
/// object and not a slice of it; and the shape it evaluates to equal to the
/// shape it was declared with, so the extents are literal on both sides and a
/// caller comparing two of these is comparing whole objects.
fn whole_array<'view>(reference: &'view TypedReferenceView<'_>) -> Option<&'view [usize]> {
    if reference.rank == 0 {
        return None;
    }
    let declared = reference.declared_extents.as_deref()?;
    if reference.extents.as_deref()? != declared {
        return None;
    }
    parts_of(reference)
        .iter()
        .all(|part| part.subscripts.is_empty())
        .then_some(declared)
}

/// The name of a whole declared array a **local** reference names.
fn whole_local<'a>(reference: &TypedReferenceView<'a>) -> Option<&'a str> {
    whole_array(reference)?;
    match &reference.node {
        TypedReferenceNodeView::Local(part) => Some(part.name.lexeme()),
        TypedReferenceNodeView::State(_) => None,
    }
}

fn parts_of<'view, 'a>(reference: &'view TypedReferenceView<'a>) -> &'view [TypedRefPartView<'a>] {
    match &reference.node {
        TypedReferenceNodeView::Local(part) => std::slice::from_ref(part),
        TypedReferenceNodeView::State(parts) => parts.as_slice(),
    }
}

/// A whole-array copy, in either of the two forms a body spells one.
struct WholeCopy<'a> {
    target: TypedReferenceView<'a>,
    source: TypedReferenceView<'a>,
}

/// The whole-array copy a statement performs, if that is all it does.
///
/// A statement a kernel already claimed is refused: its emission is that
/// kernel's, not this assignment's, and the two rewrites must not both own one
/// statement.
fn whole_copy<'a>(statement: &TypedSpannedStatement<'a>) -> Option<WholeCopy<'a>> {
    if statement.kernel.is_some() {
        return None;
    }
    match &statement.node {
        TypedStatementView::Assignment { target, value } => {
            let source = reference_of(value)?;
            if whole_array(target)? != whole_array(source)?
                || target.scalar.is_none()
                || target.scalar != source.scalar
            {
                return None;
            }
            Some(WholeCopy {
                target: target.clone(),
                source: source.clone(),
            })
        }
        TypedStatementView::For(_) => loop_copy(statement),
        _ => None,
    }
}

/// The whole-array copy a perfect loop nest performs, if that is all it does.
///
/// The nest has to traverse the *whole* declared object on both sides, once
/// each, in ascending order: every level a `for i in 1:n` over a distinct
/// iterator with no step and no declarations of its own, exactly one statement
/// per level, and an innermost element assignment whose two operands are
/// subscripted by those iterators in that order and by nothing else. That is
/// precisely the nest whose effect is `target := source`.
fn loop_copy<'a>(statement: &TypedSpannedStatement<'a>) -> Option<WholeCopy<'a>> {
    let mut iterators: Vec<&'a ast::Name> = Vec::new();
    let mut extents: Vec<usize> = Vec::new();
    let mut node = &statement.node;
    loop {
        match node {
            TypedStatementView::For(loop_view) => {
                let iterator = loop_view.iterator.as_ref()?;
                if loop_view.step.is_some()
                    || !loop_view.c_locals.is_empty()
                    || loop_view.body.len() != 1
                    || !matches!(loop_view.start.node, TypedExpressionNodeView::Integer(1))
                    || iterators
                        .iter()
                        .any(|seen| seen.lexeme() == iterator.lexeme())
                {
                    return None;
                }
                let TypedExpressionNodeView::Integer(stop) = loop_view.stop.node else {
                    return None;
                };
                let inner = loop_view.body.first()?;
                if inner.kernel.is_some() {
                    return None;
                }
                iterators.push(iterator);
                extents.push(usize::try_from(stop).ok()?);
                node = &inner.node;
            }
            TypedStatementView::Assignment { target, value } => {
                if iterators.is_empty() {
                    return None;
                }
                let source = reference_of(value)?;
                if target.scalar.is_none() || target.scalar != source.scalar {
                    return None;
                }
                let target = whole_object(target, &iterators, &extents)?;
                let source = whole_object(source, &iterators, &extents)?;
                return Some(WholeCopy { target, source });
            }
            _ => return None,
        }
    }
}

/// The whole declared object an element reference names, given the iterators
/// that subscript it, or `None` when the element is not the nest's complete
/// traversal of a declared array.
fn whole_object<'a>(
    element: &TypedReferenceView<'a>,
    iterators: &[&'a ast::Name],
    extents: &[usize],
) -> Option<TypedReferenceView<'a>> {
    if element.rank != 0 || element.declared_extents.as_deref()? != extents {
        return None;
    }
    let mut object = element.clone();
    {
        let parts: &mut [TypedRefPartView<'a>] = match &mut object.node {
            TypedReferenceNodeView::Local(part) => std::slice::from_mut(part),
            TypedReferenceNodeView::State(parts) => parts.as_mut_slice(),
        };
        let (last, leading) = parts.split_last_mut()?;
        if !leading.iter().all(|part| part.subscripts.is_empty())
            || last.subscripts.len() != iterators.len()
            || !last
                .subscripts
                .iter()
                .zip(iterators)
                .all(|(subscript, iterator)| is_iterator_reference(subscript, iterator))
        {
            return None;
        }
        last.subscripts.clear();
    }
    object.rank = extents.len();
    object.extents = Some(extents.to_vec());
    Some(object)
}

/// How many times a body names each local, counted over exactly the references
/// a target still prints.
///
/// An absorbed statement prints nothing, so it contributes nothing; a statement
/// a kernel claimed contributes both its own references and the kernel's, which
/// over-counts a `fill` and can only cost a forwarding.
#[derive(Default)]
struct Census<'a> {
    counts: HashMap<&'a str, usize>,
    /// Set by a construct this census cannot read, which makes every count in
    /// it a lower bound. A body that sets it is refused outright: a signal
    /// check's fallback is a checked expression this view never projected, so a
    /// local it names would be invisible here.
    opaque: bool,
}

impl<'a> Census<'a> {
    fn of(statements: &[TypedSpannedStatement<'a>]) -> Self {
        let mut census = Self::default();
        census.observe_statements(statements);
        census
    }

    fn of_statement(statement: &TypedSpannedStatement<'a>) -> Self {
        let mut census = Self::default();
        census.observe_statements(std::slice::from_ref(statement));
        census
    }

    fn count_of(&self, name: &str) -> usize {
        self.counts.get(name).copied().unwrap_or_default()
    }

    fn observe_statements(&mut self, statements: &[TypedSpannedStatement<'a>]) {
        for statement in statements {
            if matches!(statement.kernel, Some(KernelStatementView::Absorbed)) {
                continue;
            }
            match &statement.kernel {
                // A marshalled call prints its own statement and nothing of the
                // node beside it, so the node's operands are counted nowhere.
                Some(KernelStatementView::Marshalled { statement }) => {
                    self.observe_statement(statement);
                    continue;
                }
                None | Some(KernelStatementView::Absorbed) => {}
                Some(KernelStatementView::Fill { value }) => self.observe_expression(value),
                Some(KernelStatementView::Dot {
                    target, lhs, rhs, ..
                }) => {
                    self.observe_reference(target);
                    self.observe_reference(lhs);
                    self.observe_reference(rhs);
                }
                Some(KernelStatementView::ScaledAdd {
                    zero,
                    target,
                    iterator,
                    scale,
                    source,
                    ..
                }) => {
                    self.observe_expression(zero);
                    self.observe_reference(target);
                    self.observe_name(iterator);
                    self.observe_expression(scale);
                    self.observe_reference(source);
                }
            }
            self.observe_statement(&statement.node);
        }
    }

    fn observe_statement(&mut self, statement: &TypedStatementView<'a>) {
        match statement {
            TypedStatementView::Assignment { target, value } => {
                self.observe_reference(target);
                self.observe_expression(value);
            }
            TypedStatementView::MultiAssignment { targets, call } => {
                for target in targets {
                    self.observe_reference(target);
                }
                self.observe_call(call);
            }
            TypedStatementView::Call(call) => self.observe_call(call),
            TypedStatementView::If(conditional) => self.observe_conditional(conditional),
            TypedStatementView::For(loop_view) => {
                if let Some(iterator) = loop_view.iterator.as_ref() {
                    self.observe_name(iterator);
                }
                self.observe_expression(&loop_view.start);
                self.observe_expression(&loop_view.stop);
                if let Some(step) = &loop_view.step {
                    self.observe_expression(step);
                }
                self.observe_statements(&loop_view.body);
            }
            TypedStatementView::Limit(targets) => self.observe_limit(targets),
            TypedStatementView::Signal(_) => {}
        }
    }

    fn observe_conditional(&mut self, conditional: &super::TypedIfStatementView<'a>) {
        for branch in &conditional.branches {
            self.observe_condition(&branch.condition);
            self.observe_statements(&branch.body);
        }
        if let Some(body) = &conditional.else_body {
            self.observe_statements(body);
        }
    }

    fn observe_condition(&mut self, condition: &TypedConditionView<'a>) {
        match condition {
            TypedConditionView::Expression(value) => self.observe_expression(value),
            // A signal check's fallback is a checked expression this view never
            // projected, so a local it names would be invisible to the count.
            TypedConditionView::SignalCheck(check) => self.opaque |= check.fallback.is_some(),
        }
    }

    fn observe_limit(&mut self, targets: &[TypedLimitTargetView<'a>]) {
        for target in targets {
            // `limit self` names no local at all: it saturates ranged block
            // state, which is never a forwarded aggregate.
            if let TypedLimitTargetView::Reference(reference) = target {
                self.observe_reference(reference);
            }
        }
    }

    fn observe_call(&mut self, call: &TypedCallView<'a>) {
        for argument in &call.arguments {
            self.observe_expression(argument);
        }
    }

    fn observe_reference(&mut self, reference: &TypedReferenceView<'a>) {
        if let TypedReferenceNodeView::Local(part) = &reference.node {
            self.observe_name(part.name);
        }
        for part in parts_of(reference) {
            for subscript in &part.subscripts {
                self.observe_expression(subscript);
            }
        }
    }

    fn observe_name(&mut self, name: &'a ast::Name) {
        *self.counts.entry(name.lexeme()).or_default() += 1;
    }

    fn observe_expression(&mut self, value: &TypedExpressionView<'a>) {
        match &value.node {
            TypedExpressionNodeView::Bool(_)
            | TypedExpressionNodeView::Integer(_)
            | TypedExpressionNodeView::Real(_) => {}
            TypedExpressionNodeView::Ref(reference) | TypedExpressionNodeView::Neg(reference) => {
                self.observe_reference(reference);
            }
            TypedExpressionNodeView::Size { array, dimension } => {
                self.observe_reference(array);
                self.observe_expression(dimension);
            }
            TypedExpressionNodeView::Call(call) => self.observe_call(call),
            TypedExpressionNodeView::Paren(inner) | TypedExpressionNodeView::Not(inner) => {
                self.observe_expression(inner);
            }
            TypedExpressionNodeView::If(conditional) => {
                for (condition, value) in &conditional.branches {
                    self.observe_expression(condition);
                    self.observe_expression(value);
                }
                self.observe_expression(&conditional.else_value);
            }
            TypedExpressionNodeView::BoundedSelection(selection) => {
                self.observe_reference(&selection.reference);
                for (condition, value) in &selection.galec.branches {
                    self.observe_expression(condition);
                    self.observe_expression(value);
                }
                self.observe_expression(&selection.galec.else_value);
            }
            TypedExpressionNodeView::Array(elements) => {
                for element in elements {
                    self.observe_expression(element);
                }
            }
            TypedExpressionNodeView::Binary { lhs, rhs, .. } => {
                self.observe_expression(lhs);
                self.observe_expression(rhs);
            }
        }
    }
}

/// Permission to drop one marshalling copy, and the only place it is minted.
///
/// Nothing outside this module can build a [`Forwarding`]: its fields are
/// private here and [`forward_results`] and [`forward_arguments`] are the only
/// expressions that construct one. Every obligation below is discharged in the
/// same expression that mints the value, so a rewrite cannot run ahead of its
/// proof.
mod permission {
    use std::collections::HashSet;

    use super::{
        Census, TypedReferenceView, TypedSpannedStatement, emittable, emitted, user_call,
        whole_array, whole_copy, whole_local,
    };

    /// Where a forwarded aggregate goes in the rewritten call statement.
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub(super) enum Position {
        /// Result slot `n` of the call, which the read-back writes.
        Target(usize),
        /// Actual parameter `n` of the call, which the callee reads.
        Actual(usize),
    }

    /// Evidence that one marshalling copy may be dropped, together with the
    /// rewrite that replaces it.
    ///
    /// # Theorem (a forwarding preserves every emitted value)
    ///
    /// Let `F` be a `Forwarding` minted for a temporary `t`, an aggregate `o`,
    /// a copy statement at `copy`, and a call statement at `call`, both in one
    /// statement list `L`. Applying `F`, which names `o` at `F.position` in
    /// `L[call]` and absorbs `L[copy]`, leaves every object other than `t`
    /// holding the value it held before, at every point after `L[call]`.
    ///
    /// *Proof.* Both minting sites establish, for their direction:
    ///
    /// 1. `t` is named exactly twice in the whole body (`census`), once in
    ///    `L[copy]` and once in `L[call]`. So no statement other than these two
    ///    reads or writes `t`, and `t` is dead after `L[call]`; a value only
    ///    `t` holds is observed by nothing.
    /// 2. `o` and `t` are distinct local declarations of the calling owner, and
    ///    `o` is the whole declared object, with extents and element type equal
    ///    to `t`'s. So the copy transfers exactly `o`'s elements, in ascending
    ///    order, onto exactly `t`'s.
    /// 3. No statement strictly between `copy` and `call` names `o`.
    ///
    /// For [`forward_arguments`], `L[copy]` is `t := o` and `L[call]` reads `t`
    /// as an actual. By (2) `t` and `o` hold equal elements after `L[copy]`, and
    /// by (3) and (1) neither is written before `L[call]`, so the callee reads
    /// equal elements from either. The callee cannot write `o`: `o` is a local
    /// of the caller, and a callee writes only its own region, its callees'
    /// regions and block state (see the module note). By (1) `t` is then dead.
    ///
    /// For [`forward_results`], `L[call]` writes `t` at result slot `n` and
    /// `L[copy]` is `o := t`. Writing `o` at slot `n` rather than `t` moves
    /// `o`'s write earlier, to a point after the callee has returned. Nothing
    /// between observes it, by (3); no other result slot of the same call names
    /// `o`, which the minting site checks, so no later read-back overwrites it;
    /// and the value written is the same read-back value the copy transferred,
    /// by (2). By (1) `t` is then dead.
    ///
    /// In both cases the surviving statement is emitted where the call already
    /// was, so no ordering among the remaining statements changes. ∎
    pub(super) struct Forwarding<'a> {
        copy: usize,
        call: usize,
        position: Position,
        object: TypedReferenceView<'a>,
        object_root: &'a str,
        retired: &'a str,
    }

    impl<'a> Forwarding<'a> {
        pub(super) const fn copy_index(&self) -> usize {
            self.copy
        }

        pub(super) const fn call_index(&self) -> usize {
            self.call
        }

        pub(super) const fn position(&self) -> Position {
            self.position
        }

        pub(super) const fn object_root(&self) -> &'a str {
            self.object_root
        }

        pub(super) const fn retired(&self) -> &'a str {
            self.retired
        }

        pub(super) fn into_object(self) -> TypedReferenceView<'a> {
            self.object
        }
    }

    /// Whether a temporary is a retirable local named exactly twice in the
    /// body, once inside the statement that is about to be rewritten.
    fn sole_marshalling_use<'a>(
        temporary: &TypedReferenceView<'a>,
        census: &Census<'a>,
        here: &Census<'a>,
        retirable: &HashSet<&'a str>,
    ) -> Option<&'a str> {
        let name = whole_local(temporary)?;
        (retirable.contains(name) && census.count_of(name) == 2 && here.count_of(name) == 1)
            .then_some(name)
    }

    /// Whether two whole-array references have the same shape and element type,
    /// exactly.
    fn same_shape(left: &TypedReferenceView<'_>, right: &TypedReferenceView<'_>) -> bool {
        whole_array(left).is_some()
            && whole_array(left) == whole_array(right)
            && left.scalar.is_some()
            && left.scalar == right.scalar
    }

    /// Mint a forwarding for every result of the call at `call_index` whose
    /// only use is a whole-array copy into another local aggregate.
    pub(super) fn forward_results<'a>(
        list: &[TypedSpannedStatement<'a>],
        per_statement: &[Census<'a>],
        census: &Census<'a>,
        retirable: &HashSet<&'a str>,
        call_index: usize,
    ) -> Vec<Forwarding<'a>> {
        let mut minted = Vec::new();
        if !emittable(&list[call_index]) {
            return minted;
        }
        let Some((_, targets)) = user_call(emitted(&list[call_index])) else {
            return minted;
        };
        for (index, target) in targets.iter().enumerate() {
            let Some(name) =
                sole_marshalling_use(target, census, &per_statement[call_index], retirable)
            else {
                continue;
            };
            // The one remaining mention is the first statement after the call
            // that names the temporary; it has to be the copy itself.
            let Some(copy_index) = (call_index + 1..list.len())
                .find(|position| per_statement[*position].count_of(name) > 0)
            else {
                continue;
            };
            let Some(copy) = whole_copy(&list[copy_index]) else {
                continue;
            };
            if whole_local(&copy.source) != Some(name) || !same_shape(&copy.target, target) {
                continue;
            }
            let Some(destination) = whole_local(&copy.target) else {
                continue;
            };
            // A destination that is another result slot of this same call would
            // be overwritten by that slot's read-back, which runs after this
            // one.
            let clashes = targets
                .iter()
                .enumerate()
                .any(|(other, slot)| other != index && whole_local(slot) == Some(destination));
            if destination == name
                || clashes
                || (call_index + 1..copy_index)
                    .any(|position| per_statement[position].count_of(destination) > 0)
            {
                continue;
            }
            minted.push(Forwarding {
                copy: copy_index,
                call: call_index,
                position: Position::Target(index),
                object: copy.target,
                object_root: destination,
                retired: name,
            });
        }
        minted
    }

    /// Mint a forwarding for every actual of the call at `call_index` that is a
    /// temporary whose only definition is a whole-array copy of another local
    /// aggregate.
    pub(super) fn forward_arguments<'a>(
        list: &[TypedSpannedStatement<'a>],
        per_statement: &[Census<'a>],
        census: &Census<'a>,
        retirable: &HashSet<&'a str>,
        call_index: usize,
    ) -> Vec<Forwarding<'a>> {
        let mut minted = Vec::new();
        if !emittable(&list[call_index]) {
            return minted;
        }
        let Some((call, targets)) = user_call(emitted(&list[call_index])) else {
            return minted;
        };
        for (index, argument) in call.arguments.iter().enumerate() {
            let Some(actual) = super::reference_of(argument) else {
                continue;
            };
            let Some(name) =
                sole_marshalling_use(actual, census, &per_statement[call_index], retirable)
            else {
                continue;
            };
            // The one remaining mention is the last statement before the call
            // that names the temporary; it has to be the staging copy itself.
            let Some(copy_index) = (0..call_index)
                .rev()
                .find(|position| per_statement[*position].count_of(name) > 0)
            else {
                continue;
            };
            let Some(copy) = whole_copy(&list[copy_index]) else {
                continue;
            };
            if whole_local(&copy.target) != Some(name) || !same_shape(&copy.source, actual) {
                continue;
            }
            let Some(source) = whole_local(&copy.source) else {
                continue;
            };
            // A source that is also a result slot of this call is refused: the
            // read-back would write the object the callee has just read, which
            // is correct but is not an argument this proof makes.
            let clashes = targets.iter().any(|slot| whole_local(slot) == Some(source));
            if source == name
                || clashes
                || (copy_index + 1..call_index)
                    .any(|position| per_statement[position].count_of(source) > 0)
            {
                continue;
            }
            minted.push(Forwarding {
                copy: copy_index,
                call: call_index,
                position: Position::Actual(index),
                object: copy.source,
                object_root: source,
                retired: name,
            });
        }
        minted
    }
}
