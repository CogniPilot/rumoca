//! Collapsing the tensor-assignment axes that only select a coordinate.
//!
//! An indexed Modelica assignment such as `a[i, :] := row` keeps the whole of
//! `a` as its value in DAE, so its projection answers, at every coordinate of
//! `a`, either the new row or the historical element. Lowered as written that
//! walks the full rank: the row axis iterates its whole extent and each
//! iteration tests `i == row` to decide which of the two it meant. Every
//! iteration the test rejects reads one element and writes the same element
//! back.
//!
//! Those iterations are a no-op, so the axis does not need a loop at all: the
//! assignment writes the single coordinate `i`. Recognizing that collapses the
//! axis's whole extent to one, and takes the equality test with it. The
//! recognition is a property of the emitted element body, not of any
//! particular model: it fires wherever an assignment's own subscript picks the
//! coordinate an axis is iterating over.
//!
//! Nothing here is heuristic. The collapse is taken only when the body proves
//! all five of:
//!
//! * the element store writes exactly the coordinate the nest iterates;
//! * the guard's false arm writes that same element straight back, so the
//!   iterations being dropped are stores of a value to itself;
//! * the guard names the axis iterator against an expression no iterator in
//!   the nest can change, so the coordinate it selects is the same on every
//!   iteration;
//! * that expression is proven to land inside the axis, which is what makes
//!   writing it directly a store the loop would have made; and
//! * substituting it for the iterator reaches every occurrence.
//!
//! A body that fails any of them keeps its loop.
//!
//! The bounds conjunct is not a formality. Written as a guard, a coordinate
//! outside the axis simply never matches and the assignment writes nothing;
//! written as a direct store, the same coordinate writes outside the array.
//! The lowering that emits these guards proves the bound before it emits them,
//! so the fact exists -- but it is a fact about that lowering, not about the
//! statements this module reads, so [`AxisBounds`] carries it here rather than
//! letting the shape imply it.

use super::*;

/// The result of collapsing an element body's selector axes.
pub(in crate::lower) struct SelectorCollapse {
    /// Positions in the nest's iterator list whose loops are now unnecessary.
    pub(in crate::lower) axes: Vec<usize>,
    /// The element body with those iterators bound to the coordinates the
    /// assignment selects and the discharged equalities removed.
    pub(in crate::lower) body: Vec<gast::Spanned<gast::Statement>>,
}

/// One axis the guard pins to a single coordinate.
struct SelectorEquality {
    axis: usize,
    selected: gast::Expression,
}

/// The nest's axes, and the proof that a coordinate lands inside one.
///
/// `proven` is the lowering's own dynamic-index proof, passed in rather than
/// re-derived: it is the same question `a[i, :] := row` had to answer about
/// `i` before it could be lowered at all, and answering it twice from two
/// different places is how the two answers come to disagree.
pub(in crate::lower) struct AxisBounds<'a> {
    /// Extent of each axis, in the nest's iterator order.
    pub(in crate::lower) extents: &'a [u32],
    /// Whether an expression is proven to lie within `1 ..= extent`.
    pub(in crate::lower) proven: &'a dyn Fn(&gast::Expression, u32) -> bool,
}

impl AxisBounds<'_> {
    /// Whether `coordinate` is proven to be a coordinate of `axis`.
    fn admits(&self, axis: usize, coordinate: &gast::Expression) -> bool {
        self.extents
            .get(axis)
            .is_some_and(|&extent| (self.proven)(coordinate, extent))
    }
}

/// The shape a guarded element body has to have before any axis of it can be
/// collapsed: one guard over one store, whose false arm is the store's own
/// element.
struct GuardedElementStore {
    condition: gast::Expression,
    /// Statements the guard takes before the store, if the guard needed any.
    taken: Vec<gast::Spanned<gast::Statement>>,
    store: gast::Spanned<gast::Statement>,
}

/// Collapse every axis of `body` that the assignment's own subscripts pin to a
/// single coordinate.
///
/// Returns `None` when the body is not a guarded element store, when the guard
/// pins no axis, when a pinned coordinate is not proven to lie inside its axis,
/// or when the substitution cannot reach an occurrence of an iterator it would
/// bind.
pub(in crate::lower) fn collapse_selector_axes(
    body: &[gast::Spanned<gast::Statement>],
    iterators: &[gast::Name],
    bounds: &AxisBounds<'_>,
) -> Option<SelectorCollapse> {
    let guarded = guarded_element_store(body, iterators)?;
    let (selectors, residual) =
        partition_selector_equalities(&guarded.condition, iterators, bounds)?;
    if selectors.is_empty() {
        return None;
    }
    let bound = selectors
        .iter()
        .map(|selector| iterators[selector.axis].clone())
        .collect::<Vec<_>>();
    if declares_iterator(&guarded.taken, &bound) {
        return None;
    }
    let span = guarded.store.span;
    let mut body = guarded.taken;
    body.push(guarded.store);
    if let Some(residual) = residual {
        body = vec![guard_once(residual, body, span)];
    }
    let selected = selectors
        .iter()
        .map(|selector| selector.selected.clone())
        .collect::<Vec<_>>();
    let mut binder = Binder {
        bound: &bound,
        selected: &selected,
        blocked: false,
    };
    binder.body(&mut body);
    if binder.blocked {
        return None;
    }
    Some(SelectorCollapse {
        axes: selectors
            .into_iter()
            .map(|selector| selector.axis)
            .collect(),
        body,
    })
}

/// Wrap a body in a guard with no else arm, which is what the conjuncts that
/// pin no axis become once the pinned ones are discharged.
fn guard_once(
    condition: gast::Expression,
    body: Vec<gast::Spanned<gast::Statement>>,
    span: Span,
) -> gast::Spanned<gast::Statement> {
    gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(condition),
                body,
                span,
            }],
            else_body: None,
        }),
        span,
    )
}

/// Match the emitted shape of an indexed assignment's element: a guard whose
/// false arm leaves the element the store writes exactly as it found it.
///
/// That false arm is the whole proof that the rejected iterations may be
/// dropped. An iteration the guard rejects loads one element and stores the
/// identical value into it, so deleting it leaves the same memory. Requiring
/// the restored and the stored reference to be equal as written keeps the
/// reasoning structural: no aliasing question is asked, because both name the
/// same element.
///
/// A selection reaches the element in one of two shapes, depending on whether
/// its taken arm needed statements of its own, and both say the same thing.
fn guarded_element_store(
    body: &[gast::Spanned<gast::Statement>],
    iterators: &[gast::Name],
) -> Option<GuardedElementStore> {
    match body {
        [store] => restoring_conditional_store(store, iterators),
        [guard, store] => restoring_guarded_store(guard, store, iterators),
        _ => None,
    }
}

/// The statement shape: `if c then … s := v else s := t[i] end if; t[i] := s`.
fn restoring_guarded_store(
    guard: &gast::Spanned<gast::Statement>,
    store: &gast::Spanned<gast::Statement>,
    iterators: &[gast::Name],
) -> Option<GuardedElementStore> {
    let gast::Statement::If(guard) = &guard.node else {
        return None;
    };
    let [branch] = guard.branches.as_slice() else {
        return None;
    };
    let gast::Condition::Expression(condition) = &branch.condition else {
        return None;
    };
    let gast::Statement::Assignment {
        target: stored,
        value: gast::Expression::Ref(gast::Reference::Local(selection)),
    } = &store.node
    else {
        return None;
    };
    if !selection.subscripts.is_empty() || !writes_element_coordinates(stored, iterators) {
        return None;
    }
    let [restore] = guard.else_body.as_ref()?.as_slice() else {
        return None;
    };
    let gast::Statement::Assignment {
        target: gast::Reference::Local(restored),
        value: gast::Expression::Ref(historical),
    } = &restore.node
    else {
        return None;
    };
    if !same_name(&restored.name, &selection.name)
        || !restored.subscripts.is_empty()
        || !same_element(historical, stored, iterators)
    {
        return None;
    }
    Some(GuardedElementStore {
        condition: condition.clone(),
        taken: branch.body.clone(),
        store: store.clone(),
    })
}

/// The expression shape: `t[i] := if c then v else t[i]`.
///
/// A conditional carrying a bounded-selection correlation is refused: its
/// legalized twin would have to be rebuilt around the collapsed coordinate,
/// and a selection that reaches its element by one equality per subscript is
/// not the indexed update this collapse is about.
fn restoring_conditional_store(
    store: &gast::Spanned<gast::Statement>,
    iterators: &[gast::Name],
) -> Option<GuardedElementStore> {
    let gast::Statement::Assignment {
        target: stored,
        value: gast::Expression::If(selection),
    } = &store.node
    else {
        return None;
    };
    if selection.bounded_selection_correlation().is_some()
        || !writes_element_coordinates(stored, iterators)
    {
        return None;
    }
    let [(condition, taken)] = selection.branches.as_slice() else {
        return None;
    };
    if !matches!(
        selection.else_value.as_ref(),
        gast::Expression::Ref(historical) if same_element(historical, stored, iterators)
    ) {
        return None;
    }
    Some(GuardedElementStore {
        condition: condition.clone(),
        taken: Vec::new(),
        store: gast::Spanned::new(
            gast::Statement::Assignment {
                target: stored.clone(),
                value: taken.clone(),
            },
            store.span,
        ),
    })
}

/// Whether a store names exactly the coordinate the nest iterates, in order.
///
/// Anything else -- an offset walk, a transposed one, a partly literal
/// coordinate -- means binding an iterator would not redirect this store to
/// the coordinate the guard selects, so the axis keeps its loop.
fn writes_element_coordinates(target: &gast::Reference, iterators: &[gast::Name]) -> bool {
    let parts = match target {
        gast::Reference::Local(part) => std::slice::from_ref(part),
        gast::Reference::State(parts) => parts,
    };
    let Some((last, prefix)) = parts.split_last() else {
        return false;
    };
    if prefix.iter().any(|part| !part.subscripts.is_empty())
        || last.subscripts.len() != iterators.len()
    {
        return false;
    }
    last.subscripts
        .iter()
        .zip(iterators)
        .all(|(subscript, iterator)| is_iterator_reference(subscript, iterator))
}

fn is_iterator_reference(expression: &gast::Expression, iterator: &gast::Name) -> bool {
    matches!(
        expression,
        gast::Expression::Ref(gast::Reference::Local(part))
            if part.subscripts.is_empty() && same_name(&part.name, iterator)
    )
}

/// Two names denote the same entity when their lexemes agree.
///
/// The derived equality on a name and on a reference part also compares the
/// source span each carries, and the two references this module has to match
/// are built at different points of one lowering, so they carry different
/// spans for the same entity. Provenance is not identity here.
fn same_name(lhs: &gast::Name, rhs: &gast::Name) -> bool {
    lhs.lexeme() == rhs.lexeme()
}

/// Whether two references name the same element, given both write the nest's
/// coordinates.
///
/// The coordinates are already pinned to the iterators by
/// [`writes_element_coordinates`], so agreeing on the dotted name path is what
/// remains of element identity.
fn same_element(lhs: &gast::Reference, rhs: &gast::Reference, iterators: &[gast::Name]) -> bool {
    if !writes_element_coordinates(lhs, iterators) || !writes_element_coordinates(rhs, iterators) {
        return false;
    }
    let (lhs, rhs) = match (lhs, rhs) {
        (gast::Reference::Local(lhs), gast::Reference::Local(rhs)) => {
            (std::slice::from_ref(lhs), std::slice::from_ref(rhs))
        }
        (gast::Reference::State(lhs), gast::Reference::State(rhs)) => {
            (lhs.as_slice(), rhs.as_slice())
        }
        _ => return false,
    };
    lhs.len() == rhs.len()
        && lhs
            .iter()
            .zip(rhs)
            .all(|(lhs, rhs)| same_name(&lhs.name, &rhs.name))
}

/// Whether an expression reads any of the nest's iterators, by lexeme.
///
/// The shared dependence helper matches names including their spans, which is
/// exactly right where the names come from one minting and wrong here, where
/// the guard is built from a different one. Missing a read would let an
/// iterator be bound to a coordinate that varies with it, so this walk asks
/// the weaker, safer question.
fn reads_any_iterator(expression: &gast::Expression, iterators: &[gast::Name]) -> bool {
    let mut found = false;
    read_expression(expression, &mut |name| {
        found |= iterators.iter().any(|iterator| same_name(name, iterator));
    });
    found
}

fn read_expression(expression: &gast::Expression, visit: &mut impl FnMut(&gast::Name)) {
    match expression {
        gast::Expression::Bool(_) | gast::Expression::Integer(_) | gast::Expression::Real(_) => {}
        gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
            read_reference(reference, visit);
        }
        gast::Expression::Size { array, dimension } => {
            read_reference(array, visit);
            read_expression(dimension, visit);
        }
        gast::Expression::Call(call) => {
            for argument in &call.arguments {
                read_expression(argument, visit);
            }
        }
        gast::Expression::Paren(value) | gast::Expression::Not(value) => {
            read_expression(value, visit);
        }
        gast::Expression::If(value) => {
            for (condition, branch) in &value.branches {
                read_expression(condition, visit);
                read_expression(branch, visit);
            }
            read_expression(&value.else_value, visit);
        }
        gast::Expression::Array(values) => {
            for value in values {
                read_expression(value, visit);
            }
        }
        gast::Expression::Binary { lhs, rhs, .. } => {
            read_expression(lhs, visit);
            read_expression(rhs, visit);
        }
    }
}

fn read_reference(reference: &gast::Reference, visit: &mut impl FnMut(&gast::Name)) {
    let parts = match reference {
        gast::Reference::Local(part) => std::slice::from_ref(part),
        gast::Reference::State(parts) => parts,
    };
    for part in parts {
        visit(&part.name);
        for subscript in &part.subscripts {
            read_expression(subscript, visit);
        }
    }
}

/// Split a guard into the equalities that pin one axis each and the rest.
///
/// A conjunct pins an axis when it compares that axis's iterator against an
/// expression no iterator in the nest can change: the coordinate it admits is
/// then the same on every iteration of the nest, which is what makes binding
/// the iterator to it faithful. Conjuncts that pin nothing -- the range tests a
/// slice subscript contributes, for one -- are handed back to guard the
/// collapsed body, where they still decide whether the store happens at all.
///
/// Returns `None` when two conjuncts pin the same axis, which is a guard this
/// reasoning does not cover rather than one it may assume consistent.
fn partition_selector_equalities(
    condition: &gast::Expression,
    iterators: &[gast::Name],
    bounds: &AxisBounds<'_>,
) -> Option<(Vec<SelectorEquality>, Option<gast::Expression>)> {
    let mut selectors: Vec<SelectorEquality> = Vec::new();
    let mut residual: Option<gast::Expression> = None;
    for conjunct in conjuncts(condition) {
        match selector_equality(conjunct, iterators, bounds) {
            Some(selector) => {
                if selectors.iter().any(|held| held.axis == selector.axis) {
                    return None;
                }
                selectors.push(selector);
            }
            None => {
                residual = Some(match residual {
                    Some(held) => {
                        gast::Expression::binary(gast::BinaryOp::And, held, conjunct.clone())
                    }
                    None => conjunct.clone(),
                });
            }
        }
    }
    Some((selectors, residual))
}

fn conjuncts(condition: &gast::Expression) -> Vec<&gast::Expression> {
    match condition {
        gast::Expression::Paren(inner) => conjuncts(inner),
        gast::Expression::Binary {
            op: gast::BinaryOp::And,
            lhs,
            rhs,
        } => {
            let mut terms = conjuncts(lhs);
            terms.extend(conjuncts(rhs));
            terms
        }
        _ => vec![condition],
    }
}

fn selector_equality(
    conjunct: &gast::Expression,
    iterators: &[gast::Name],
    bounds: &AxisBounds<'_>,
) -> Option<SelectorEquality> {
    let gast::Expression::Binary {
        op: gast::BinaryOp::Eq,
        lhs,
        rhs,
    } = conjunct
    else {
        return None;
    };
    pinned_axis(lhs, rhs, iterators, bounds).or_else(|| pinned_axis(rhs, lhs, iterators, bounds))
}

fn pinned_axis(
    iterator: &gast::Expression,
    selected: &gast::Expression,
    iterators: &[gast::Name],
    bounds: &AxisBounds<'_>,
) -> Option<SelectorEquality> {
    let axis = iterators
        .iter()
        .position(|name| is_iterator_reference(iterator, name))?;
    if reads_any_iterator(selected, iterators) || !bounds.admits(axis, selected) {
        return None;
    }
    Some(SelectorEquality {
        axis,
        selected: selected.clone(),
    })
}

/// Whether any loop inside the body declares one of the iterators being bound.
///
/// Substitution assumes an iterator means the same thing everywhere it appears
/// in the body. A nested loop that redeclares the name would break that, so a
/// body holding one keeps its axis.
fn declares_iterator(body: &[gast::Spanned<gast::Statement>], bound: &[gast::Name]) -> bool {
    body.iter().any(|statement| match &statement.node {
        gast::Statement::For(value) => {
            value
                .iterator
                .as_ref()
                .is_some_and(|iterator| bound.iter().any(|bound| same_name(bound, iterator)))
                || declares_iterator(&value.body, bound)
        }
        gast::Statement::If(value) => {
            value
                .branches
                .iter()
                .any(|branch| declares_iterator(&branch.body, bound))
                || value
                    .else_body
                    .as_ref()
                    .is_some_and(|body| declares_iterator(body, bound))
        }
        _ => false,
    })
}

/// Replaces every reference to a bound iterator with the coordinate the
/// assignment selects, recording whether any reference sat where the
/// replacement cannot reach.
struct Binder<'a> {
    bound: &'a [gast::Name],
    selected: &'a [gast::Expression],
    blocked: bool,
}

impl Binder<'_> {
    fn coordinate(&self, name: &gast::Name) -> Option<&gast::Expression> {
        let position = self.bound.iter().position(|bound| same_name(bound, name))?;
        self.selected.get(position)
    }

    fn body(&mut self, body: &mut [gast::Spanned<gast::Statement>]) {
        for statement in body {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &mut gast::Spanned<gast::Statement>) {
        match &mut statement.node {
            gast::Statement::Assignment { target, value } => {
                self.reference(target);
                self.expression(value);
            }
            gast::Statement::MultiAssignment { targets, call } => {
                for target in targets {
                    self.reference(target);
                }
                self.call(call);
            }
            gast::Statement::Call(call) => self.call(call),
            gast::Statement::If(value) => self.if_statement(value),
            gast::Statement::For(value) => self.for_loop(value),
            gast::Statement::Limit(targets) => self.limits(targets),
            gast::Statement::Signal(_) => {}
        }
    }

    fn limits(&mut self, targets: &mut [gast::LimitTarget]) {
        for target in targets {
            if let gast::LimitTarget::Reference(reference) = target {
                self.reference(reference);
            }
        }
    }

    fn if_statement(&mut self, value: &mut gast::IfStatement) {
        for branch in &mut value.branches {
            self.condition(&mut branch.condition);
            self.body(&mut branch.body);
        }
        if let Some(body) = &mut value.else_body {
            self.body(body);
        }
    }

    fn condition(&mut self, condition: &mut gast::Condition) {
        match condition {
            gast::Condition::Expression(expression) => self.expression(expression),
            gast::Condition::SignalCheck(check) => {
                if let Some(fallback) = &mut check.fallback {
                    self.expression(fallback);
                }
            }
        }
    }

    fn for_loop(&mut self, value: &mut gast::ForLoop) {
        self.expression(&mut value.start);
        if let Some(step) = &mut value.step {
            self.expression(step);
        }
        self.expression(&mut value.stop);
        self.body(&mut value.body);
    }

    fn call(&mut self, call: &mut gast::FunctionCall) {
        for argument in &mut call.arguments {
            self.expression(argument);
        }
    }

    fn expression(&mut self, expression: &mut gast::Expression) {
        match expression {
            gast::Expression::Bool(_)
            | gast::Expression::Integer(_)
            | gast::Expression::Real(_) => {}
            gast::Expression::Ref(reference) => {
                if let Some(coordinate) = self.bound_reference(reference) {
                    *expression = coordinate;
                    return;
                }
                self.reference(reference);
            }
            gast::Expression::Neg(reference) => self.negated(reference),
            gast::Expression::Size { array, dimension } => {
                self.reference(array);
                self.expression(dimension);
            }
            gast::Expression::Call(call) => self.call(call),
            gast::Expression::Paren(value) | gast::Expression::Not(value) => {
                self.expression(value);
            }
            gast::Expression::If(value) => self.conditional(value),
            gast::Expression::Array(values) => {
                for value in values {
                    self.expression(value);
                }
            }
            gast::Expression::Binary { lhs, rhs, .. } => {
                self.expression(lhs);
                self.expression(rhs);
            }
        }
    }

    /// A negation holds a reference rather than an expression, so it can only
    /// take a coordinate that is itself a reference. Anything else blocks the
    /// collapse instead of being rebuilt into a shape the operand cannot hold.
    fn negated(&mut self, reference: &mut gast::Reference) {
        match self.bound_reference(reference) {
            Some(gast::Expression::Ref(coordinate)) => *reference = coordinate,
            Some(_) => self.blocked = true,
            None => self.reference(reference),
        }
    }

    /// A conditional expression may carry a legalized twin beside its
    /// branches, and binding an iterator in the branches alone would leave the
    /// two disagreeing. One that carries a twin therefore blocks the collapse
    /// as soon as it names a bound iterator; one that carries none has nothing
    /// to keep in step and is rewritten like any other operand.
    fn conditional(&mut self, value: &mut gast::IfExpression) {
        if value.bounded_selection_correlation().is_some() {
            if reads_any_iterator(&gast::Expression::If(value.clone()), self.bound) {
                self.blocked = true;
            }
            return;
        }
        for (condition, branch) in &mut value.branches {
            self.expression(condition);
            self.expression(branch);
        }
        self.expression(&mut value.else_value);
    }

    fn bound_reference(&self, reference: &gast::Reference) -> Option<gast::Expression> {
        let gast::Reference::Local(part) = reference else {
            return None;
        };
        if !part.subscripts.is_empty() {
            return None;
        }
        self.coordinate(&part.name).cloned()
    }

    fn reference(&mut self, reference: &mut gast::Reference) {
        let parts = match reference {
            gast::Reference::Local(part) => std::slice::from_mut(part),
            gast::Reference::State(parts) => parts.as_mut_slice(),
        };
        for part in parts {
            if self.coordinate(&part.name).is_some() {
                // An array named by a bound iterator cannot occur: iterators
                // are scalars this pass mints. Refuse rather than assume it.
                self.blocked = true;
            }
            for subscript in &mut part.subscripts {
                self.expression(subscript);
            }
        }
    }
}

#[cfg(test)]
mod tests;
