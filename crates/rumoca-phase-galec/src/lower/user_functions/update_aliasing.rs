//! Whether a function value's stores reach the reads that name what it held
//! before them.
//!
//! # The hazard this module owns
//!
//! GALEC has statements, not values. Every checked function value the DAE
//! commits becomes storage, and every read of that value becomes a read of the
//! storage where the reading code sits. The DAE, by contrast, states each value
//! against the content its own definition named, and it does not always give
//! that content a name: a scratch a guard nest writes gets no total owner, so
//! the checked conditional drops it from its target list and substitutes its
//! right-hand side into every sibling of the group that reads it. Each of those
//! siblings then *expands* that right-hand side at its own store.
//!
//! An expansion is faithful exactly when the storage it reads still holds what
//! the definition it came from named. Everything below is that one question,
//! asked wherever a store can land between a definition and an expansion of it.
//! There are exactly two such places, because there are exactly two ways GALEC
//! interleaves a store with a read:
//!
//! 1. **Inside one assignment.** A tensor assignment becomes an element loop
//!    that stores one coordinate per iteration, so a later iteration can read
//!    an element an earlier one already wrote. The per-axis placement proof
//!    below decides it, and [`SnapshotRewrite::rewrite`] repairs it by
//!    materializing the pre-assignment aggregate.
//! 2. **Across the assignments of one group.** A checked group commits its
//!    definitions atomically, and GALEC emits them one store at a time, so a
//!    slot emitted later can read storage an earlier slot has replaced.
//!    [`order_by_value_dependency`] decides it, repairs it by choosing an
//!    emission order, and refuses the group when no order does.
//!
//! # Why those two places are all of them
//!
//! A read can only be stale if a store landed between the definition it names
//! and the point it is expanded at. Function construction issues one definition
//! per emitted assignment and threads reads to the definition current where the
//! source read them, so a read whose definition is the one storage currently
//! holds is not stale by construction. That leaves exactly the stores emitted
//! between: the ones a single assignment's own element loop performs (1), and
//! the ones its group siblings perform (2). Nothing else can intervene, because
//! the substitution that makes a definition expandable at all is confined to
//! one group: the checked conditional builds one branch-local value environment,
//! consumes it while lowering that branch, and commits only the group's
//! declared targets, so a value it drops cannot be read outside the group that
//! dropped it.
//!
//! # Which test pins each instance
//!
//! Every instance of this class that has been found live is pinned by a
//! three-leg differential fixture in `suite_galec_fmu::galec_equivalence`:
//!
//! * a shared elimination factor expanded into the row update that overwrites
//!   its own divisor, repaired by emission order:
//!   `embedded_c_elimination_assigns_the_shared_factor_before_the_row_it_overwrites`;
//! * a whole-aggregate exchange whose element loop reads rows it has already
//!   stored, repaired by the snapshot:
//!   `embedded_c_exchanged_rows_read_the_value_the_assignment_started_from`;
//! * a scratch a guard nest writes, expanded inside the update that overwrites
//!   it and again in a second aggregate's update that follows it, spelled both
//!   as a whole scalar and as an array element:
//!   `embedded_c_expands_a_substituted_scratch_where_the_storage_it_reads_still_holds_it`.
//!
//! The last of these is why the decision lives in one module rather than in the
//! analysis that hands out owners: whether a scratch keeps an owner depends on
//! how the source spelled it, and this decision may not. It is stated over the
//! stores GALEC actually emits, so it sees the expansion whatever produced it.
//!
//! The refusal has a witness of its own, because a decision that can fail
//! closed is only worth trusting if the closed path is exercised: two
//! eliminations whose scale factors cross leave each slot expanding a read of
//! what the other stores, and `suite_galec_fmu::galec_store_order` pins that
//! such a group is refused rather than emitted in one of the two wrong orders.
//!
//! # What places a read
//!
//! The loop stores, at coordinate `c`, either the update's value (when `c` is
//! inside the region the assignment names) or the element the target already
//! held there (when it is not). Storage outside the named region is therefore
//! unchanged for the whole loop, and storage inside it changes only when the
//! loop reaches that coordinate. A read at coordinate `d`, evaluated while
//! coordinate `c` is being stored, therefore sees the pre-assignment value
//! whenever `d` is `c` itself or `d` lies outside the region.
//!
//! Both are established one axis at a time, and neither needs to know which
//! one ends up holding:
//!
//! * an axis the assignment stores at a *single* coordinate needs nothing from
//!   the read at all. Either the read's coordinate on that axis is the stored
//!   one, and the read agrees with `c` there, or it is not, and `d` is already
//!   outside the region on that axis alone;
//! * every other axis is stored at many coordinates, so the read must name the
//!   coordinate being stored: its subscript has to be the axis's loop iterator,
//!   as integer arithmetic ([`same_index`]), not as text.
//!
//! When every axis clears one of the two, either each one took the first case
//! of its rule (and then `d == c`), or some single-coordinate axis took its
//! second case, and then `d` is outside the region. This is what keeps the
//! elimination shapes in place:
//! `y[row, column:n] := y[row, column:n] - factor * y[column, column:n]`
//! stores one row, so the row axis carries no obligation, and both reads walk
//! the column axis with the iterator the store uses.
//!
//! # Why the reads are counted after projection
//!
//! The obligation is about the statements the loop body ends up holding, not
//! about the value the DAE wrote. Projection hoists what does not vary with the
//! element (the shared elimination factor is assigned once ahead of the loop),
//! and a read hoisted out of the loop runs before the first store, so it needs
//! no proof. Counting the DAE's occurrences instead would charge the assignment
//! for reads its emitted form performs exactly once, before it stores anything.
//!
//! The one fact that cannot be recovered after projection is which axes are
//! stored at a single coordinate: projection replaces the source subscripts
//! with coordinate arithmetic over the iterators. That fact alone is read off
//! the checked DAE, by [`single_coordinate_axes`].

use super::*;
use indexed_update::preserves_function_target;

/// The storage a tensor assignment stores into, as the DAE names it.
pub(super) struct UpdatedAggregate<'dae> {
    /// The function value whose storage the emitted loop writes.
    pub(super) value: dae::FunctionValueView<'dae>,
    /// That storage's declared rank.
    pub(super) rank: usize,
}

/// Which axes of the target the assignment stores at one coordinate.
///
/// An axis qualifies when every update the value stacks names it with a single
/// index and they all name the same one, and when every branch of the value
/// ends at the target's own current value. A branch ending anywhere else
/// assigns the whole aggregate, so nothing outside the named region stays
/// unchanged and no axis carries the exemption.
pub(super) fn single_coordinate_axes<'dae>(
    view: dae::DaeView<'dae>,
    target: &UpdatedAggregate<'dae>,
    expression: dae::ExprId<'dae>,
) -> Vec<bool> {
    let mut region = StoredRegion {
        view,
        target,
        axes: vec![None; target.rank],
        first: true,
        exempt: true,
        budget: MAX_REGION_NODES,
    };
    region.walk(expression, 0);
    if !region.exempt {
        return vec![false; target.rank];
    }
    region.axes.iter().map(Option::is_some).collect()
}

/// The axes of the region a value's stacked updates store into, accumulated
/// over every branch.
struct StoredRegion<'a, 'dae> {
    view: dae::DaeView<'dae>,
    target: &'a UpdatedAggregate<'dae>,
    /// Per axis, the single stored coordinate every update seen so far names.
    axes: Vec<Option<dae::ExprId<'dae>>>,
    /// No update has been folded in yet, so `axes` carries no claim.
    first: bool,
    /// Every branch ends at the target's own value, which is what makes
    /// storage outside the region unchanged.
    exempt: bool,
    /// Spine nodes left to visit before the walk stops trusting itself.
    budget: usize,
}

/// How deep a stack of updates one value may carry before the region walk
/// stops trusting itself. A function statement writes one region; a stack this
/// tall is not a shape this exemption was reasoned about.
const MAX_STACKED_UPDATES: usize = 8;

/// How many spine nodes the region walk visits before refusing the exemption.
/// The spine is the conditional structure of one statement's value, so this
/// bounds the walk without reaching any shape the exemption is claimed for.
const MAX_REGION_NODES: usize = 4096;

impl<'dae> StoredRegion<'_, 'dae> {
    fn walk(&mut self, expression: dae::ExprId<'dae>, depth: usize) {
        let Some(budget) = self.budget.checked_sub(1) else {
            self.exempt = false;
            return;
        };
        self.budget = budget;
        if depth > MAX_STACKED_UPDATES {
            self.exempt = false;
            return;
        }
        let Some(node) = self.view.expression(expression) else {
            self.exempt = false;
            return;
        };
        match node.operation() {
            dae::ExpressionOperation::Conditional(operands) if operands.len() == 3 => {
                self.walk(operands.get(1).expect("checked conditional value"), depth);
                self.walk(
                    operands.get(2).expect("checked conditional fallback"),
                    depth,
                );
            }
            dae::ExpressionOperation::ArrayUpdate {
                base, subscripts, ..
            } => {
                self.fold_in(subscripts);
                self.walk(base, depth + 1);
            }
            _ => {
                self.exempt &= preserves_function_target(self.view, self.target.value, expression);
            }
        }
    }

    fn fold_in(&mut self, subscripts: dae::SubscriptsView<'dae>) {
        for axis in 0..self.target.rank {
            let stored = match subscripts.get(axis) {
                Some(dae::SubscriptView::Index { expression, .. }) => Some(expression),
                _ => None,
            };
            let kept = match (self.first, self.axes[axis], stored) {
                (true, _, stored) => stored,
                (false, Some(seen), Some(stored)) if same_value(self.view, seen, stored) => {
                    Some(seen)
                }
                _ => None,
            };
            self.axes[axis] = kept;
        }
        self.first = false;
    }
}

/// The node budget [`same_value`] spends before answering "different".
///
/// A stored index is small (a binder, a literal, a sum of the two), so a walk
/// this long has already left the shapes the comparison exists for.
const SAME_VALUE_BUDGET: usize = 128;

/// Whether two DAE expressions denote the same value.
///
/// Node identity alone is not enough: two occurrences of one source subscript
/// carry their own spans and can enter the arena as separate nodes.
fn same_value<'dae>(
    view: dae::DaeView<'dae>,
    left: dae::ExprId<'dae>,
    right: dae::ExprId<'dae>,
) -> bool {
    let mut budget = SAME_VALUE_BUDGET;
    same_value_within(view, left, right, &mut budget)
}

fn same_value_within<'dae>(
    view: dae::DaeView<'dae>,
    left: dae::ExprId<'dae>,
    right: dae::ExprId<'dae>,
    budget: &mut usize,
) -> bool {
    if left == right {
        return true;
    }
    if *budget == 0 {
        return false;
    }
    *budget -= 1;
    let (Some(left), Some(right)) = (view.expression(left), view.expression(right)) else {
        return false;
    };
    if left.value_type_id() != right.value_type_id() {
        return false;
    }
    match (left.operation(), right.operation()) {
        (dae::ExpressionOperation::Literal(left), dae::ExpressionOperation::Literal(right)) => {
            left == right
        }
        (
            dae::ExpressionOperation::Coordinate(left),
            dae::ExpressionOperation::Coordinate(right),
        ) => left == right,
        (
            dae::ExpressionOperation::Unary {
                operator: left_operator,
                operand: left_operand,
            },
            dae::ExpressionOperation::Unary {
                operator: right_operator,
                operand: right_operand,
            },
        ) => {
            left_operator == right_operator
                && same_value_within(view, left_operand, right_operand, budget)
        }
        (
            dae::ExpressionOperation::Binary {
                operator: left_operator,
                lhs: left_lhs,
                rhs: left_rhs,
            },
            dae::ExpressionOperation::Binary {
                operator: right_operator,
                lhs: right_lhs,
                rhs: right_rhs,
            },
        ) => {
            left_operator == right_operator
                && same_value_within(view, left_lhs, right_lhs, budget)
                && same_value_within(view, left_rhs, right_rhs, budget)
        }
        _ => false,
    }
}

/// One assignment's aliasing question, and the rewrite that answers it.
pub(super) struct SnapshotRewrite<'a> {
    /// The local the element loop stores into.
    pub(super) target: &'a gast::Name,
    /// The loop's per-axis iterator index, in axis order.
    pub(super) indices: &'a [gast::Expression],
    /// Per axis, whether the assignment stores that axis at one coordinate.
    pub(super) single_coordinate: &'a [bool],
}

impl SnapshotRewrite<'_> {
    /// Whether any read of the target inside the loop names storage the loop's
    /// own earlier stores may have changed.
    pub(super) fn stores_reach_the_reads(
        &self,
        value: &gast::Expression,
        body: &[gast::Spanned<gast::Statement>],
    ) -> bool {
        let mut unplaced = false;
        self.visit_expression(value, &mut unplaced);
        for statement in body {
            self.visit_statement(&statement.node, &mut unplaced);
        }
        unplaced
    }

    /// Whether one read of the target names the element the loop is storing,
    /// or storage the loop never changes.
    ///
    /// Both per-axis rules of the module's proof are applied here; see the
    /// module documentation for why one of them holding on every axis is
    /// enough.
    fn read_is_placed(&self, part: &gast::RefPart) -> bool {
        if part.name.lexeme() != self.target.lexeme() {
            return true;
        }
        if part.subscripts.len() != self.indices.len() {
            return false;
        }
        part.subscripts
            .iter()
            .zip(self.indices)
            .enumerate()
            .all(|(axis, (read, stored))| {
                self.single_coordinate.get(axis).copied().unwrap_or(false)
                    || same_index(read, stored)
            })
    }

    /// A block-state reference names storage of the enclosing block, never a
    /// function local, so only the local form can be this assignment's target.
    fn visit_reference(&self, reference: &gast::Reference, unplaced: &mut bool) {
        let parts = match reference {
            gast::Reference::Local(part) => {
                *unplaced |= !self.read_is_placed(part);
                std::slice::from_ref(part)
            }
            gast::Reference::State(parts) => parts,
        };
        for part in parts {
            for subscript in &part.subscripts {
                self.visit_expression(subscript, unplaced);
            }
        }
    }

    /// Visit every reference the expression reads.
    ///
    /// This is [`any_expression`] with a predicate that never decides anything
    /// and records instead: the reference-bearing nodes are handled here
    /// because a reference is not an expression the walk could offer, and every
    /// other node is left to it.
    fn visit_expression(&self, expression: &gast::Expression, unplaced: &mut bool) {
        any_expression(expression, &mut |node| match node {
            gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
                self.visit_reference(reference, unplaced);
                Some(false)
            }
            gast::Expression::Size { array, dimension } => {
                self.visit_reference(array, unplaced);
                self.visit_expression(dimension, unplaced);
                Some(false)
            }
            _ => None,
        });
    }

    fn visit_call(&self, call: &gast::FunctionCall, unplaced: &mut bool) {
        for argument in &call.arguments {
            self.visit_expression(argument, unplaced);
        }
    }

    fn visit_statement(&self, statement: &gast::Statement, unplaced: &mut bool) {
        match statement {
            gast::Statement::Assignment { target, value } => {
                self.visit_reference(target, unplaced);
                self.visit_expression(value, unplaced);
            }
            gast::Statement::MultiAssignment { targets, call } => {
                for target in targets {
                    self.visit_reference(target, unplaced);
                }
                self.visit_call(call, unplaced);
            }
            gast::Statement::Call(call) => self.visit_call(call, unplaced),
            gast::Statement::If(value) => self.visit_if(value, unplaced),
            gast::Statement::For(value) => {
                self.visit_expression(&value.start, unplaced);
                if let Some(step) = value.step.as_ref() {
                    self.visit_expression(step, unplaced);
                }
                self.visit_expression(&value.stop, unplaced);
                for nested in &value.body {
                    self.visit_statement(&nested.node, unplaced);
                }
            }
            gast::Statement::Limit(targets) => {
                for reference in limited_references(targets) {
                    self.visit_reference(reference, unplaced);
                }
            }
            gast::Statement::Signal(_) => {}
        }
    }

    fn visit_if(&self, value: &gast::IfStatement, unplaced: &mut bool) {
        for branch in &value.branches {
            if let gast::Condition::Expression(condition) = &branch.condition {
                self.visit_expression(condition, unplaced);
            }
            for nested in &branch.body {
                self.visit_statement(&nested.node, unplaced);
            }
        }
        for nested in value.else_body.iter().flatten() {
            self.visit_statement(&nested.node, unplaced);
        }
    }

    /// Read the target through `snapshot` at every read the proof left
    /// unplaced.
    ///
    /// A placed read stays on the aggregate: it names the element the store is
    /// about to overwrite, which still holds its pre-assignment value when the
    /// read runs, and leaving it keeps the identity restores the selector-axis
    /// collapse recognizes.
    pub(super) fn rewrite(
        &self,
        snapshot: &gast::Name,
        value: &mut gast::Expression,
        body: &mut [gast::Spanned<gast::Statement>],
    ) {
        self.rewrite_expression(value, snapshot);
        self.rewrite_statements(body, snapshot);
    }

    fn rewrite_statements(
        &self,
        statements: &mut [gast::Spanned<gast::Statement>],
        snapshot: &gast::Name,
    ) {
        for statement in statements {
            self.rewrite_statement(&mut statement.node, snapshot);
        }
    }

    fn rewrite_reference(&self, reference: &mut gast::Reference, snapshot: &gast::Name) {
        let parts = match reference {
            gast::Reference::Local(part) => {
                if !self.read_is_placed(part) {
                    part.name = snapshot.clone();
                }
                std::slice::from_mut(part)
            }
            gast::Reference::State(parts) => parts.as_mut_slice(),
        };
        for part in parts {
            for subscript in &mut part.subscripts {
                self.rewrite_expression(subscript, snapshot);
            }
        }
    }

    fn rewrite_expression(&self, expression: &mut gast::Expression, snapshot: &gast::Name) {
        match expression {
            gast::Expression::Bool(_)
            | gast::Expression::Integer(_)
            | gast::Expression::Real(_) => {}
            gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
                self.rewrite_reference(reference, snapshot);
            }
            gast::Expression::Size { array, dimension } => {
                self.rewrite_reference(array, snapshot);
                self.rewrite_expression(dimension, snapshot);
            }
            gast::Expression::Call(call) => self.rewrite_call(call, snapshot),
            gast::Expression::Paren(value) | gast::Expression::Not(value) => {
                self.rewrite_expression(value, snapshot);
            }
            gast::Expression::If(value) => self.rewrite_if_expression(value, snapshot),
            gast::Expression::Array(values) => {
                for value in values {
                    self.rewrite_expression(value, snapshot);
                }
            }
            gast::Expression::Binary { lhs, rhs, .. } => {
                self.rewrite_expression(lhs, snapshot);
                self.rewrite_expression(rhs, snapshot);
            }
        }
    }

    /// Restate a bounded selection over the snapshot, expansion included.
    ///
    /// A dynamic element read keeps the tensor selection it stands for beside
    /// its GALEC expansion (SPEC_0034 GAL-033), and the two are checked against
    /// each other. Rewriting the expansion alone would leave the selection
    /// naming an aggregate its own branches no longer read, so the selection is
    /// restated and its expansion derived from it again. The selection's
    /// subscripts are the same on both sides, so it moves exactly when its
    /// branches do.
    fn rewrite_if_expression(&self, value: &mut gast::IfExpression, snapshot: &gast::Name) {
        if let Some(correlation) = value.bounded_selection_correlation() {
            let mut reference = correlation.reference().clone();
            let extents = correlation.extents().to_vec();
            self.rewrite_reference(&mut reference, snapshot);
            if let Ok(rebuilt) = gast::IfExpression::bounded_selection(reference, extents) {
                *value = rebuilt;
                return;
            }
        }
        for (condition, branch) in &mut value.branches {
            self.rewrite_expression(condition, snapshot);
            self.rewrite_expression(branch, snapshot);
        }
        self.rewrite_expression(&mut value.else_value, snapshot);
    }

    fn rewrite_call(&self, call: &mut gast::FunctionCall, snapshot: &gast::Name) {
        for argument in &mut call.arguments {
            self.rewrite_expression(argument, snapshot);
        }
    }

    fn rewrite_statement(&self, statement: &mut gast::Statement, snapshot: &gast::Name) {
        match statement {
            gast::Statement::Assignment { target, value } => {
                self.rewrite_subscripts(target, snapshot);
                self.rewrite_expression(value, snapshot);
            }
            gast::Statement::MultiAssignment { targets, call } => {
                for target in targets {
                    self.rewrite_subscripts(target, snapshot);
                }
                self.rewrite_call(call, snapshot);
            }
            gast::Statement::Call(call) => self.rewrite_call(call, snapshot),
            gast::Statement::If(value) => self.rewrite_if_statement(value, snapshot),
            gast::Statement::For(value) => {
                self.rewrite_expression(&mut value.start, snapshot);
                if let Some(step) = value.step.as_mut() {
                    self.rewrite_expression(step, snapshot);
                }
                self.rewrite_expression(&mut value.stop, snapshot);
                self.rewrite_statements(&mut value.body, snapshot);
            }
            gast::Statement::Limit(targets) => {
                for reference in limited_references_mut(targets) {
                    self.rewrite_subscripts(reference, snapshot);
                }
            }
            gast::Statement::Signal(_) => {}
        }
    }

    fn rewrite_if_statement(&self, value: &mut gast::IfStatement, snapshot: &gast::Name) {
        for branch in &mut value.branches {
            if let gast::Condition::Expression(condition) = &mut branch.condition {
                self.rewrite_expression(condition, snapshot);
            }
            self.rewrite_statements(&mut branch.body, snapshot);
        }
        if let Some(body) = value.else_body.as_mut() {
            self.rewrite_statements(body, snapshot);
        }
    }

    /// Rewrite only the subscripts of a reference, never the name it stores
    /// into: the loop keeps storing to the aggregate.
    fn rewrite_subscripts(&self, reference: &mut gast::Reference, snapshot: &gast::Name) {
        let parts = match reference {
            gast::Reference::Local(part) => std::slice::from_mut(part),
            gast::Reference::State(parts) => parts.as_mut_slice(),
        };
        for part in parts {
            for subscript in &mut part.subscripts {
                self.rewrite_expression(subscript, snapshot);
            }
        }
    }
}

/// The entities a `limit` statement names, skipping the `self` form, which
/// carries no reference of its own.
fn limited_references(targets: &[gast::LimitTarget]) -> impl Iterator<Item = &gast::Reference> {
    targets.iter().filter_map(|limited| match limited {
        gast::LimitTarget::SelfState => None,
        gast::LimitTarget::Reference(reference) => Some(reference),
    })
}

fn limited_references_mut(
    targets: &mut [gast::LimitTarget],
) -> impl Iterator<Item = &mut gast::Reference> {
    targets.iter_mut().filter_map(|limited| match limited {
        gast::LimitTarget::SelfState => None,
        gast::LimitTarget::Reference(reference) => Some(reference),
    })
}

/// One subscript's value as `scale * atom + offset`, when it is that.
struct AffineIndex<'a> {
    atom: Option<&'a gast::Reference>,
    scale: i64,
    offset: i64,
}

/// Whether two GALEC subscripts evaluate to the same coordinate.
///
/// Projection writes a slice walk as offset arithmetic over the loop iterator
/// (`1 + ((i - 0 - 1) * 1)`), so the read that walks the stored axis coordinate
/// for coordinate is not textually the store's subscript. Comparing the two as
/// integer arithmetic is what recognizes them as one coordinate.
///
/// The same question decides whether an element read of a tensor SSA update
/// selects the updated value or the base it was stacked on, so
/// [`ExpressionLowerer::lower_array_update_at`] asks it here rather than
/// emitting `i == i` and leaving a dead branch in the generated code.
pub(in crate::lower) fn same_index(left: &gast::Expression, right: &gast::Expression) -> bool {
    let (Some(left), Some(right)) = (affine_index(left), affine_index(right)) else {
        return false;
    };
    if left.scale != right.scale || left.offset != right.offset {
        return false;
    }
    match (left.atom, right.atom) {
        (None, None) => true,
        (Some(left), Some(right)) => same_atom(left, right),
        _ => false,
    }
}

/// Whether two subscript atoms name the same declaration at the same
/// coordinate.
///
/// Names and subscripts decide it; the source span a reference carries does
/// not, because the same iterator is spanned differently wherever projection
/// planted it. A block-state reference is never a subscript atom in a function
/// body and is reported as different rather than compared part by part.
fn same_atom(left: &gast::Reference, right: &gast::Reference) -> bool {
    let (gast::Reference::Local(left), gast::Reference::Local(right)) = (left, right) else {
        return false;
    };
    left.name.lexeme() == right.name.lexeme()
        && left.subscripts.len() == right.subscripts.len()
        && left
            .subscripts
            .iter()
            .zip(&right.subscripts)
            .all(|(left, right)| same_index(left, right))
}

/// The affine form of one integer subscript, over at most one reference.
///
/// Anything else (a call, a selection, a second distinct reference) has no
/// affine form here and is reported as unplaceable, which costs a snapshot and
/// never a wrong answer.
fn affine_index(expression: &gast::Expression) -> Option<AffineIndex<'_>> {
    match expression {
        gast::Expression::Integer(value) => Some(AffineIndex {
            atom: None,
            scale: 0,
            offset: *value,
        }),
        gast::Expression::Ref(reference) => Some(AffineIndex {
            atom: Some(reference),
            scale: 1,
            offset: 0,
        }),
        gast::Expression::Neg(reference) => Some(AffineIndex {
            atom: Some(reference),
            scale: -1,
            offset: 0,
        }),
        gast::Expression::Paren(value) => affine_index(value),
        gast::Expression::Binary { op, lhs, rhs } => affine_binary(*op, lhs, rhs),
        _ => None,
    }
}

fn affine_binary<'a>(
    op: gast::BinaryOp,
    lhs: &'a gast::Expression,
    rhs: &'a gast::Expression,
) -> Option<AffineIndex<'a>> {
    let lhs = affine_index(lhs)?;
    let rhs = affine_index(rhs)?;
    match op {
        gast::BinaryOp::Add => affine_sum(lhs, rhs),
        gast::BinaryOp::Sub => affine_sum(lhs, affine_negated(rhs)?),
        gast::BinaryOp::Mul => affine_product(lhs, rhs),
        _ => None,
    }
}

fn affine_sum<'a>(lhs: AffineIndex<'a>, rhs: AffineIndex<'a>) -> Option<AffineIndex<'a>> {
    let offset = lhs.offset.checked_add(rhs.offset)?;
    match (lhs.atom, rhs.atom) {
        (None, atom) | (atom, None) => Some(AffineIndex {
            atom,
            scale: lhs.scale.checked_add(rhs.scale)?,
            offset,
        }),
        (Some(left), Some(right)) if same_atom(left, right) => Some(AffineIndex {
            atom: Some(left),
            scale: lhs.scale.checked_add(rhs.scale)?,
            offset,
        }),
        _ => None,
    }
}

fn affine_negated(value: AffineIndex<'_>) -> Option<AffineIndex<'_>> {
    Some(AffineIndex {
        atom: value.atom,
        scale: value.scale.checked_neg()?,
        offset: value.offset.checked_neg()?,
    })
}

fn affine_product<'a>(lhs: AffineIndex<'a>, rhs: AffineIndex<'a>) -> Option<AffineIndex<'a>> {
    let (constant, other) = match (lhs.atom, rhs.atom) {
        (None, _) => (lhs.offset, rhs),
        (_, None) => (rhs.offset, lhs),
        _ => return None,
    };
    Some(AffineIndex {
        atom: other.atom,
        scale: other.scale.checked_mul(constant)?,
        offset: other.offset.checked_mul(constant)?,
    })
}

/// Declare the local one assignment materializes its pre-assignment aggregate
/// into.
///
/// The local carries the target's own declared shape, which is the smallest
/// object that answers every read the proof could not place: an unplaced read
/// is one whose coordinate the projection no longer names, so no narrower
/// slice is derivable from it. The scratch overlay folds the local into the
/// frame it shares with the other per-assignment temporaries.
pub(super) fn declare_snapshot(
    lowerer: &mut ExpressionLowerer<'_, '_>,
    extents: &[u32],
    scalar: gast::ScalarType,
    span: Span,
) -> gast::Name {
    let name = gast::Name::ident(format!(
        "rumoca_{}_snapshot_{}",
        lowerer.temporary_namespace, lowerer.temporary_counter
    ));
    lowerer.temporary_counter += 1;
    lowerer.temporary_locals.push(gast::VariableDeclaration {
        ty: gast::TypeRef::Primitive(scalar),
        name: name.clone(),
        dimensions: user_functions::dimensions(extents),
        range: gast::RangeAttributes::default(),
        span,
    });
    name
}

/// The shape one assignment's snapshot local carries.
pub(super) struct SnapshotShape<'a> {
    pub(super) extents: &'a [u32],
    pub(super) scalar: gast::ScalarType,
    pub(super) span: Span,
}

/// Materialize the pre-assignment aggregate when the loop's stores reach its
/// reads, and hand back the statements that must run before the loop.
///
/// Returns an empty sequence when every read is placed, which is the shape
/// every assignment that does not read what it stores keeps.
pub(super) fn snapshot_prologue(
    lowerer: &mut ExpressionLowerer<'_, '_>,
    rewrite: &SnapshotRewrite<'_>,
    shape: SnapshotShape<'_>,
    element: (
        &mut gast::Expression,
        &mut Vec<gast::Spanned<gast::Statement>>,
    ),
) -> Vec<gast::Spanned<gast::Statement>> {
    let (value, body) = element;
    if !rewrite.stores_reach_the_reads(value, body) {
        return Vec::new();
    }
    let snapshot = declare_snapshot(lowerer, shape.extents, shape.scalar, shape.span);
    let prologue = vec![gast::Spanned::new(
        gast::Statement::Assignment {
            target: gast::Reference::local(snapshot.clone()),
            value: gast::Expression::Ref(gast::Reference::local(rewrite.target.clone())),
        },
        shape.span,
    )];
    rewrite.rewrite(&snapshot, value, body);
    prologue
}

/// Run the pre-assignment materialization ahead of everything the assignment
/// emits.
///
/// The snapshot must be taken before the first store, and every statement the
/// assignment lifts out of its loop nest already runs against the aggregate as
/// the assignment found it, so the copy leads the whole sequence.
pub(super) fn after_snapshot(
    prologue: Vec<gast::Spanned<gast::Statement>>,
    mut lowered: LoweredTensorAssignment,
) -> LoweredTensorAssignment {
    if prologue.is_empty() {
        return lowered;
    }
    let mut before = prologue;
    before.append(&mut lowered.before);
    lowered.before = before;
    lowered
}

// ===========================================================================
// Axis two: the stores of one group's slot against the reads of its siblings.
// ===========================================================================

/// One definition of an atomically committed group, as its emission order sees
/// it: the value it assigns and the storage that assignment overwrites.
#[derive(Clone, Copy)]
pub(super) struct GroupDefinition<'dae> {
    pub(super) target: dae::FunctionValueId<'dae>,
    pub(super) value: dae::ExprId<'dae>,
    /// Where the checked statement this definition comes from sits, so a group
    /// no order discharges is reported at the source that wrote it.
    pub(super) span: Span,
}

/// Order one atomically committed group of definitions so that no assignment
/// stores over what a sibling still has to read.
///
/// The group's values are all stated against the storage the group found, so
/// emitting them one store at a time is faithful exactly when every read still
/// reaches that content where it runs. Two obligations say when it does not,
/// and each names the order that discharges it:
///
/// * a node two definitions share is lowered once and then read from the local
///   the first of them stored it in
///   ([`ExpressionLowerer::remember_primitive_assignment`]); where that node
///   reads storage this group overwrites, the slot that stores it has to go
///   first, or the reader re-expands it after that storage has moved.
///   `factor := m[r, c] / m[c, c]` beside `m[r, c:n] := m[r, c:n] - factor *
///   m[c, c:n]` is that shape: emitted the other way round the division is
///   expanded inside the loop that overwrites `m[r, c]`, so from the pivot
///   column on it divides the zero it just stored;
/// * a definition whose value reads a sibling's storage outside everything the
///   group stores is reading what that storage held before the group, so it has
///   to be emitted before the sibling stores. `y[r, c:n] := y[r, c:n] - factor
///   * y[c, c:n]` beside `z[r, :] := z[r, :] - factor * z[c, :]` is that shape
///   once `factor` has no owner of its own: `z`'s value carries its own
///   expansion of the division, which reads `y`, so storing `y` first leaves
///   `z` dividing by the zero the row update left.
///
/// Only these hazards constrain the order. A pair of definitions that share no
/// node and read nothing the other writes keeps its incoming order, so this
/// reorders no group that was already sound.
///
/// The order is checked against the obligations it was built from, and a group
/// no order discharges is refused rather than emitted: see
/// [`unplaced_group_store`].
pub(super) fn order_by_value_dependency<'dae, T: Copy>(
    view: dae::DaeView<'dae>,
    entries: &[T],
    describe: impl Fn(T) -> GroupDefinition<'dae>,
) -> Result<GroupOrder<T>, GalecTargetError> {
    let group = entries
        .iter()
        .map(|entry| describe(*entry))
        .collect::<Vec<_>>();
    let GroupHazards {
        predecessors,
        readers,
        carried,
    } = group_hazards(view, &group);
    let mut visited = vec![false; entries.len()];
    let mut ordered = Vec::with_capacity(entries.len());
    let mut slots = Vec::with_capacity(entries.len());
    let mut pending: Vec<(usize, usize)> = Vec::new();
    for start in 0..entries.len() {
        if visited[start] {
            continue;
        }
        visited[start] = true;
        pending.push((start, 0));
        while let Some((slot, cursor)) = pending.pop() {
            let Some(earlier) = predecessors[slot].get(cursor).copied() else {
                ordered.push(entries[slot]);
                slots.push(slot);
                continue;
            };
            pending.push((slot, cursor + 1));
            if !visited[earlier] {
                visited[earlier] = true;
                pending.push((earlier, 0));
            }
        }
    }
    if let Some((_, reader)) = unplaced_group_store(&slots, &readers) {
        return Err(unsupported(
            "function-group-store-order",
            "this assignment reads what a sibling assignment of the same \
checked group overwrites, and the sibling in turn needs this one's store \
first, so no emission order of the group keeps both reads"
                .to_owned(),
            group[reader].span,
        ));
    }
    Ok(GroupOrder { ordered, carried })
}

/// The first slot pair the chosen order leaves unplaced.
///
/// `readers[stored]` names the slots whose values read `stored`'s storage from
/// before the group. Emitting `stored` ahead of one of them is exactly the
/// store that reaches that read, and nothing downstream can restore what it
/// overwrote, so the pair is reported and the group is refused.
fn unplaced_group_store(slots: &[usize], readers: &[Vec<usize>]) -> Option<(usize, usize)> {
    let mut position = vec![usize::MAX; readers.len()];
    for (index, slot) in slots.iter().enumerate() {
        position[*slot] = index;
    }
    readers.iter().enumerate().find_map(|(stored, reading)| {
        reading
            .iter()
            .find(|reader| position[stored] < position[**reader])
            .map(|reader| (stored, *reader))
    })
}

/// One arm's emission order and the values its order exists to protect.
pub(super) struct GroupOrder<T> {
    pub(super) ordered: Vec<T>,
    pub(super) carried: HashSet<u32>,
}

/// The ordering obligations of one group and the shared values that raise them.
struct GroupHazards {
    /// Per slot, the slots that must be assigned before it.
    predecessors: Vec<Vec<usize>>,
    /// Per slot, the sibling slots whose values read the storage this slot
    /// overwrites, from before the group committed anything.
    readers: Vec<Vec<usize>>,
    carried: HashSet<u32>,
}

/// For each definition of one group, the slots that must be assigned before it.
///
/// Both obligations of [`order_by_value_dependency`] are collected here, and
/// they are two halves of one fact: a node one slot stores becomes that slot's
/// local, so wherever a reader carries such a node the storing slot has to go
/// first, and wherever it carries a read no slot stores the reader has to go
/// before whoever overwrites it.
///
/// The producer obligation therefore needs a shared node that reads *some*
/// slot's storage. Sharing alone is a common-subexpression fact with no
/// ordering content; a shared node that reads nothing this group writes is
/// evaluated to the same value whenever it runs. `pivot := covariance[r, c] -
/// sum(lower[r, k] * lower[c, k])` is the shape the wider reading catches: the
/// acceptance test beside it carries `pivot`'s value rather than `lower`'s, so
/// the narrow reading saw no obligation, while `pivot` storing first is what
/// lets that test read `pivot` by name instead of expanding it after `lower`
/// has been written.
///
/// The pre-group-read obligation is then stated against everything the group
/// stores, because the producer obligation is what makes each of those a name.
fn group_hazards<'dae>(view: dae::DaeView<'dae>, group: &[GroupDefinition<'dae>]) -> GroupHazards {
    let stored = group
        .iter()
        .map(|definition| stored_values(view, definition.value))
        .collect::<Vec<_>>();
    let mut predecessors = vec![Vec::new(); group.len()];
    let mut readers = vec![Vec::new(); group.len()];
    let mut carried = HashSet::new();
    for (later, definition) in group.iter().enumerate() {
        let occurring = occurring_expressions(view, definition.value);
        let named = sibling_stored_values(&stored, later);
        for (earlier, values) in stored.iter().enumerate() {
            if earlier == later {
                continue;
            }
            if reads_outside(view, definition.value, group[earlier].target, &named) {
                predecessors[earlier].push(later);
                readers[earlier].push(later);
            }
            let shared = values
                .iter()
                .copied()
                .filter(|value| {
                    occurring.contains(&value.index())
                        && group
                            .iter()
                            .any(|slot| reads_function_value(view, *value, slot.target))
                })
                .map(|value| value.index())
                .collect::<Vec<_>>();
            if shared.is_empty() {
                continue;
            }
            predecessors[later].push(earlier);
            carried.extend(shared);
        }
    }
    GroupHazards {
        predecessors,
        readers,
        carried,
    }
}

/// Everything the group's other slots store, which one slot's reader may take
/// by name instead of expanding.
///
/// The slot's own stored values are left out: its root is one of them, and it
/// is the value being walked rather than something a sibling names for it.
fn sibling_stored_values<'dae>(
    stored: &[Vec<dae::ExprId<'dae>>],
    slot: usize,
) -> Vec<dae::ExprId<'dae>> {
    stored
        .iter()
        .enumerate()
        .filter(|(ordinal, _)| *ordinal != slot)
        .flat_map(|(_, values)| values.iter().copied())
        .collect()
}

/// Every value one definition stores into its target on some path.
///
/// A conditional assignment stores one branch value per path rather than the
/// join, so it is the branch values that become locals and that a sibling can
/// read by name. This mirrors the descent
/// [`lower_conditional_function_value_assignment`] performs.
fn stored_values<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
) -> Vec<dae::ExprId<'dae>> {
    let mut stored = Vec::new();
    let mut pending = vec![root];
    while let Some(value) = pending.pop() {
        stored.push(value);
        let Some(node) = view.expression(value) else {
            continue;
        };
        let dae::ExpressionOperation::Conditional(operands) = node.operation() else {
            continue;
        };
        pending.extend(
            (1..operands.len())
                .step_by(2)
                .filter_map(|ordinal| operands.get(ordinal)),
        );
        pending.extend(
            operands
                .len()
                .checked_sub(1)
                .and_then(|fallback| operands.get(fallback)),
        );
    }
    stored
}

/// The identities of every expression node reachable from one value.
fn occurring_expressions<'dae>(view: dae::DaeView<'dae>, root: dae::ExprId<'dae>) -> HashSet<u32> {
    let mut occurring = HashSet::new();
    dae::for_each_expression(view, root, |expression, _| {
        occurring.insert(expression.index());
    });
    occurring
}

/// Whether one expression reads the storage of `target` anywhere inside it.
fn reads_function_value<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    target: dae::FunctionValueId<'dae>,
) -> bool {
    let mut reads = false;
    dae::for_each_expression(view, root, |_, node| {
        reads |= read_function_value(view, node.operation()) == Some(target);
    });
    reads
}

/// Whether one value performs a read of `target`'s storage of its own.
///
/// The walk stops wherever the emitted form reads a *name* instead of expanding
/// anything, because whatever that name's own right-hand side reads was read
/// where the name was assigned, not here:
///
/// * a node that reads a function value. Projection lowers it to that value's
///   own GALEC storage ([`ExpressionLowerer::lower_function_value`]), so the
///   node is reported when it names `target` and is never descended into. This
///   is what separates `ok := value > pivotThreshold`, whose `value` is a
///   checked assignment of its own that reads `L` once ahead of the group, from
///   a scratch with no owner, whose division over `y` is spliced into the
///   reader's value and expanded again at the reader's store;
/// * every node of `stored`, which is everything the group's *other* slots
///   store. That whole node is lowered once by the slot that stores it, and
///   every other occurrence of it reads the local that store leaves
///   ([`ExpressionLowerer::remember_primitive_assignment`]); the producer
///   obligation in [`group_hazards`] is what puts that store first, and it is
///   raised for exactly the nodes that could matter here. Where the reader is
///   emitted first anyway, it expands the node against storage that slot has
///   not written yet, so stopping is right either way.
///
/// What remains is an expansion of the content the storage carried before the
/// group, which only an emission order can place.
fn reads_outside<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    target: dae::FunctionValueId<'dae>,
    stored: &[dae::ExprId<'dae>],
) -> bool {
    let mut reads = false;
    dae::for_each_expression_pruned(view, root, |expression, node| {
        if stored.contains(&expression) {
            return false;
        }
        let Some(read) = read_function_value(view, node.operation()) else {
            return true;
        };
        reads |= read == target;
        false
    });
    reads
}

/// The function value one expression node reads, if it reads one.
///
/// A loop-carried read names its fold's target for the carried ordinal, which
/// is the same function value the enclosing group assigns.
fn read_function_value<'dae>(
    view: dae::DaeView<'dae>,
    operation: dae::ExpressionOperation<'dae>,
) -> Option<dae::FunctionValueId<'dae>> {
    let (fold, carried) = match operation {
        dae::ExpressionOperation::FunctionValue { value, .. } => return Some(value),
        dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
        | dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => (fold, carried),
        _ => return None,
    };
    view.function_fold(fold)?.targets().nth(carried as usize)
}
