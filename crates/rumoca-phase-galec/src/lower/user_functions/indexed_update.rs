//! In-place projection of a function value's element writes.
//!
//! DAE keeps an indexed Modelica assignment as a compact tensor SSA node,
//! `ArrayUpdate(base, value, subscripts)`, so a branch of an MLS §11.5
//! conditional that runs several element writes in sequence leaves *one*
//! value per target: each write wraps the aggregate the previous one
//! produced. GALEC has no such value form — it owns statements — so the
//! projection has to unwrap that value back into the statement sequence it
//! stands for. Unwrapping only the outermost level keeps the branch's last
//! write and silently drops every earlier one, which is why the whole chain
//! is peeled here.

use super::*;

/// One function value's element writes, recovered from a tensor SSA chain.
struct IndexedUpdateChain<'dae> {
    /// The peeled writes, outermost (last executed) first. Every entry is a
    /// full-rank, all-index update of the target, which is what makes it
    /// expressible as a single GALEC `target[i, j] := value;`.
    writes: Vec<(dae::ExprId<'dae>, dae::SubscriptsView<'dae>)>,
    /// The aggregate the first peeled write starts from.
    root: dae::ExprId<'dae>,
}

/// Recover every element write one tensor SSA value stands for.
///
/// Peeling stops at the first node that is not a full-rank all-index update
/// of the target: a slice, a partial subscript list, or any non-update value
/// is the aggregate the recovered writes build on, not a write itself.
fn peel_indexed_updates<'dae>(
    view: dae::DaeView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
) -> IndexedUpdateChain<'dae> {
    let mut writes = Vec::new();
    let mut root = expression;
    while let dae::ExpressionOperation::ArrayUpdate {
        base,
        value,
        subscripts,
    } = view
        .expression(root)
        .expect("checked indexed function update resolves")
        .operation()
    {
        if subscripts.len() != target_type.dimensions().len()
            || subscripts
                .iter()
                .any(|subscript| !matches!(subscript, dae::SubscriptView::Index { .. }))
        {
            break;
        }
        writes.push((value, subscripts));
        root = base;
    }
    IndexedUpdateChain { writes, root }
}

/// Decide whether the recovered writes may run in place on the target.
///
/// An in-place `target[i] := ...` sequence updates exactly the elements it
/// names and keeps every other element of whatever the target already holds.
/// That is faithful for two roots and no others:
///
/// * the target's own current value — which is what the sequence is defined
///   against in the first place;
/// * a generated function-aggregate seed — MLS §12.4.4 gives an unwritten
///   function value no initial value, and analysis mints a seed only after
///   proving the algorithm writes every declared element before anything
///   reads the value. The seed is therefore a dead store whose elements are
///   all overwritten, and the emitted algorithm must not repeat it.
///
/// Any other root is a value the algorithm really assigned to the whole
/// target. Emitting only the element writes would drop that assignment, so
/// the chain is handed back to the aggregate path, which materializes the
/// root and the updates together.
fn writes_may_run_in_place<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    root: dae::ExprId<'dae>,
) -> bool {
    if preserves_function_target(view, target, root) {
        return true;
    }
    matches!(
        view.expression(root)
            .expect("checked indexed update root resolves")
            .provenance()
            .origin(),
        dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::FunctionAggregateLowering)
    )
}

/// The elements the in-place replay has already stored, in the order it stores
/// them.
///
/// A write whose subscripts are all integer literals names exactly one element,
/// which is what makes a later read provably a different one. A write with a
/// computed subscript names an element this cannot identify, so from that point
/// on nothing can be shown to have missed the replay.
#[derive(Default)]
struct WrittenElements {
    /// One literal coordinate tuple per proven write, full-rank because peeling
    /// accepts only full-rank all-index updates.
    written: Vec<Vec<i64>>,
    /// A write landed somewhere this cannot name.
    unproven: bool,
}

impl WrittenElements {
    /// Whether the replay has stored anything yet.
    ///
    /// Before the first write, storage still holds exactly the root, so every
    /// read of it is the value the DAE names and no proof is owed.
    fn touched_anything(&self) -> bool {
        self.unproven || !self.written.is_empty()
    }

    /// Whether the replay may already have overwritten an element the read
    /// whose literal leading coordinates are `prefix` would see.
    ///
    /// A read that names fewer coordinates than the target's rank covers the
    /// whole sub-array under them — `q[1]` on a matrix is all of row 1 — so it
    /// sees a write exactly when the written tuple starts with those
    /// coordinates. For a full-rank read that degenerates to equality.
    fn may_hold(&self, prefix: &[i64]) -> bool {
        self.unproven
            || self
                .written
                .iter()
                .any(|written| written.starts_with(prefix))
    }

    fn record(&mut self, coordinates: Option<Vec<i64>>) {
        match coordinates {
            Some(coordinates) => self.written.push(coordinates),
            None => self.unproven = true,
        }
    }
}

/// The literal coordinates a subscript list names, when every one is a literal.
fn literal_coordinates<'dae>(
    view: dae::DaeView<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            dae::SubscriptView::Index { expression, .. } => literal_integer(view, expression),
            dae::SubscriptView::Slice { .. } | dae::SubscriptView::Whole { .. } => None,
        })
        .collect()
}

/// Prove that no write in the chain reads an element an earlier write of the
/// same chain has already stored.
///
/// [`writes_may_run_in_place`] proves the chain's *root* is a value the element
/// sequence may run against. That is not the same question as whether the
/// sequence's own values still see that root once it starts mutating storage,
/// and only the first was answered. A chain that writes one element and then
/// reads a *different* element of the root is fine — storage there is still the
/// root. A chain that reads an element it has already written is not: the
/// emitted `q[2] := q[1]` reads what this replay just stored, where the DAE
/// value it came from names the root's element 1.
///
/// Reads are checked in replay order — the order the emitted statements change
/// storage — and every read that cannot be placed against the writes so far is
/// treated as one that may collide. Refusing hands the chain to the aggregate
/// path, which materializes the root separately from the updates and so is
/// correct for every shape; the cost of refusing is a whole-target loop rather
/// than an element sequence.
fn writes_read_only_untouched_elements<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    chain: &IndexedUpdateChain<'dae>,
) -> bool {
    let mut written = WrittenElements::default();
    // `writes` runs outermost first, so reversing walks them in the order the
    // emitted statements execute — which is the order storage changes in.
    for (value, subscripts) in chain.writes.iter().rev() {
        // Nothing is owed before the first write, where storage still holds
        // exactly the root.
        if written.touched_anything()
            && !write_reads_untouched_storage(view, target, chain, *value, *subscripts, &written)
        {
            return false;
        }
        written.record(literal_coordinates(view, *subscripts));
    }
    true
}

/// Whether one write's value and computed subscripts both read only storage the
/// replay has not already changed.
fn write_reads_untouched_storage<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    chain: &IndexedUpdateChain<'dae>,
    value: dae::ExprId<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
    written: &WrittenElements,
) -> bool {
    if reads_replayed_storage(view, target, chain.root, value, written) {
        return false;
    }
    // A computed subscript is evaluated against the same storage the values
    // are, so it owes the same proof.
    for subscript in subscripts.iter() {
        let dae::SubscriptView::Index { expression, .. } = subscript else {
            return false;
        };
        if reads_replayed_storage(view, target, chain.root, expression, written) {
            return false;
        }
    }
    true
}

/// Whether `expression` reads the aggregate the replay is mutating at a place
/// the replay has already changed, or reads it in a way that cannot be placed.
///
/// The aggregate is named two ways: the chain's own root node, and any other
/// node denoting the same function value — a second read of the target mints a
/// distinct expression identity for the same storage, so identity alone would
/// miss it.
fn reads_replayed_storage<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    root: dae::ExprId<'dae>,
    expression: dae::ExprId<'dae>,
    written: &WrittenElements,
) -> bool {
    if expression == root {
        // A whole-value read sees every element, so any store already made is
        // visible in it.
        return true;
    }
    let Some(node) = view.expression(expression) else {
        return true;
    };
    if preserves_function_target(view, target, expression) {
        return true;
    }
    let mut children = Vec::new();
    match node.operation() {
        // An element read of the aggregate is the case worth being exact
        // about: it is proven safe exactly when no write so far landed under
        // the coordinates it names.
        dae::ExpressionOperation::Index { base, subscripts }
            if base == root || preserves_function_target(view, target, base) =>
        {
            let Some(prefix) = literal_coordinates(view, subscripts) else {
                return true;
            };
            return written.may_hold(&prefix);
        }
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Coordinate(_)
        | dae::ExpressionOperation::FunctionValue { .. }
        | dae::ExpressionOperation::FunctionFoldParameter { .. }
        | dae::ExpressionOperation::FunctionFoldOutput { .. } => {}
        dae::ExpressionOperation::Unary { operand, .. } => children.push(operand),
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => children.extend([lhs, rhs]),
        dae::ExpressionOperation::Conditional(operands)
        | dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Record(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        } => children.extend(operands.iter()),
        dae::ExpressionOperation::Call {
            owner, arguments, ..
        } => {
            children.push(owner);
            children.extend(arguments.iter());
        }
        dae::ExpressionOperation::Field { base, .. } => children.push(base),
        // MLS §10.4.1 builds a comprehension's domain from extents rather than
        // expressions, so the body is the whole of what it can read.
        dae::ExpressionOperation::Comprehension { body, .. } => children.push(body),
        dae::ExpressionOperation::Range(range) => {
            children.push(range.start().expression());
            if let Some(step) = range.explicit_step() {
                children.push(step.expression());
            }
            children.push(range.stop().expression());
        }
        dae::ExpressionOperation::Index { base, subscripts } => {
            children.push(base);
            children.extend(subscript_expressions(subscripts));
        }
        dae::ExpressionOperation::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            children.extend([base, value]);
            children.extend(subscript_expressions(subscripts));
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            children.push(value);
            match format {
                dae::StringConversionFormatView::Options {
                    minimum_length,
                    left_justified,
                    significant_digits,
                } => children.extend(
                    [minimum_length, left_justified, significant_digits]
                        .into_iter()
                        .flatten(),
                ),
                dae::StringConversionFormatView::Format { value } => children.push(value),
            }
        }
        dae::ExpressionOperation::ClockTransfer { source, .. } => children.push(source),
    }
    children
        .into_iter()
        .any(|child| reads_replayed_storage(view, target, root, child, written))
}

fn subscript_expressions<'dae>(
    subscripts: dae::SubscriptsView<'dae>,
) -> impl Iterator<Item = dae::ExprId<'dae>> {
    subscripts.iter().filter_map(|subscript| match subscript {
        dae::SubscriptView::Index { expression, .. }
        | dae::SubscriptView::Slice { expression, .. } => Some(expression),
        dae::SubscriptView::Whole { .. } => None,
    })
}

pub(super) fn lower_indexed_function_update<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    span: Span,
) -> Result<Option<Vec<gast::Spanned<gast::Statement>>>, GalecTargetError> {
    if target_type.is_record() {
        return Ok(None);
    }
    let node = view
        .expression(expression)
        .expect("checked function assignment rhs resolves");
    if let dae::ExpressionOperation::Conditional(operands) = node.operation()
        && operands.len() == 3
    {
        return lower_conditional_indexed_function_update(
            view,
            target,
            target_type,
            operands,
            lowerer,
            span,
        );
    }
    lower_indexed_function_update_expression(view, target, target_type, expression, lowerer, span)
}

fn lower_conditional_indexed_function_update<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    operands: dae::ExpressionOperands<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    span: Span,
) -> Result<Option<Vec<gast::Spanned<gast::Statement>>>, GalecTargetError> {
    let prefix_start = lowerer.pending_prefix_statements.len();
    let condition = lowerer.lower(
        operands
            .get(0)
            .expect("checked conditional update condition"),
    )?;
    require_boolean(&condition, span)?;

    let body_start = lowerer.pending_prefix_statements.len();
    let Some(updates) = lower_indexed_function_update_tree(
        view,
        target,
        target_type,
        operands.get(1).expect("checked conditional update value"),
        lowerer,
        span,
    )?
    else {
        lowerer.pending_prefix_statements.truncate(prefix_start);
        return Ok(None);
    };
    let mut body = lowerer.pending_prefix_statements.split_off(body_start);
    body.extend(updates);

    let else_start = lowerer.pending_prefix_statements.len();
    let fallback = operands
        .get(2)
        .expect("checked conditional update fallback");
    let else_body = match lower_indexed_function_update_tree(
        view,
        target,
        target_type,
        fallback,
        lowerer,
        span,
    )? {
        Some(updates) => {
            let mut statements = lowerer.pending_prefix_statements.split_off(else_start);
            statements.extend(updates);
            Some(statements)
        }
        None if preserves_function_target(view, target, fallback) => {
            lowerer.pending_prefix_statements.truncate(else_start);
            None
        }
        None => {
            lowerer.pending_prefix_statements.truncate(prefix_start);
            return Ok(None);
        }
    };
    Ok(Some(vec![gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(condition.expression),
                body,
                span,
            }],
            else_body,
        }),
        span,
    )]))
}

fn lower_indexed_function_update_tree<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    span: Span,
) -> Result<Option<Vec<gast::Spanned<gast::Statement>>>, GalecTargetError> {
    if let dae::ExpressionOperation::Conditional(operands) = view
        .expression(expression)
        .expect("checked indexed update tree resolves")
        .operation()
        && operands.len() == 3
    {
        return lower_conditional_indexed_function_update(
            view,
            target,
            target_type,
            operands,
            lowerer,
            span,
        );
    }
    lower_indexed_function_update_expression(view, target, target_type, expression, lowerer, span)
}

pub(super) fn preserves_function_target<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    match view
        .expression(expression)
        .expect("checked function fallback resolves")
        .operation()
    {
        dae::ExpressionOperation::FunctionValue { definition, .. } => {
            definition.target() == target.id()
        }
        dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
        | dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => {
            view.function_fold(fold)
                .and_then(|fold| fold.targets().nth(carried as usize))
                == Some(target.id())
        }
        dae::ExpressionOperation::Record(fields) => {
            fields.iter().enumerate().all(|(field, expression)| {
                preserves_function_target_field(view, target, expression, field)
            })
        }
        _ => false,
    }
}

fn preserves_function_target_field<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    expression: dae::ExprId<'dae>,
    expected_field: usize,
) -> bool {
    let dae::ExpressionOperation::Field { base, field } = view
        .expression(expression)
        .expect("checked function record field resolves")
        .operation()
    else {
        return false;
    };
    field as usize == expected_field && preserves_function_target(view, target, base)
}

/// Emit every element write one tensor SSA value stands for, in source order.
fn lower_indexed_function_update_expression<'a, 'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    target_type: &dae::ValueType,
    expression: dae::ExprId<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    span: Span,
) -> Result<Option<Vec<gast::Spanned<gast::Statement>>>, GalecTargetError> {
    let chain = peel_indexed_updates(view, target_type, expression);
    if chain.writes.is_empty()
        || !writes_may_run_in_place(view, target, chain.root)
        || !writes_read_only_untouched_elements(view, target, &chain)
    {
        return Ok(None);
    }
    let scalar_type = scalar_type(
        target_type.scalar_type(),
        target.name().as_str(),
        target.declaration().span(),
    )?;
    let mut statements = Vec::with_capacity(chain.writes.len());
    // `writes` runs outermost first, so the last write DAE applied is the
    // first entry. Reversing recovers the algorithm order the branch wrote.
    for (value, subscripts) in chain.writes.into_iter().rev() {
        let mut lowered_subscripts = Vec::with_capacity(subscripts.len());
        for (subscript, extent) in subscripts.iter().zip(target_type.dimensions()) {
            let dae::SubscriptView::Index { expression, .. } = subscript else {
                unreachable!("peeling accepts only all-index updates")
            };
            let index = lowerer.lower(expression)?.expression;
            if constant_integer(&index).is_none() {
                lowerer.prove_dynamic_index(&index, *extent, span)?;
            }
            lowered_subscripts.push(index);
        }
        let value = coerce(lowerer.lower(value)?, scalar_type, span)?;
        statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::Local(gast::RefPart {
                    name: value_name(target)?,
                    subscripts: lowered_subscripts,
                    span,
                }),
                value,
            },
            span,
        ));
    }
    Ok(Some(statements))
}
