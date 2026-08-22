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
        statements.extend(lower_element_write(
            lowerer,
            ElementWrite {
                target: value_name(target)?,
                subscripts: &lowered_subscripts,
                dimensions: target_type.dimensions(),
                value,
                scalar: scalar_type,
                span,
            },
        )?);
    }
    Ok(Some(statements))
}

/// One scalar element write of a function-local aggregate.
struct ElementWrite<'a> {
    target: gast::Name,
    /// One lowered subscript per declared dimension, in declaration order.
    subscripts: &'a [gast::Expression],
    /// The target's declared extents, parallel to `subscripts`.
    dimensions: &'a [u32],
    value: gast::Expression,
    scalar: gast::ScalarType,
    span: Span,
}

/// The largest number of candidate coordinates one element write may enumerate.
///
/// The expansion emits one branch per candidate, so the emitted statement count
/// is linear in this bound. A write whose dynamic axes span more coordinates is
/// refused instead of expanded into an unreviewable branch chain.
const MAX_WRITE_CANDIDATES: usize = 4096;

/// Emit one element write, expanding a proven dynamic subscript into the form
/// GALEC admits in an assignment target.
///
/// GALEC evaluates a target subscript at Production-Code-generation time
/// (§3.2.6 L-2, `EG022`), so only a literal or a loop iterator may stand there;
/// a function local may not, however well its range is proven. The read path
/// answers that requirement with a bounded selection over the extent, and this
/// is the statement form of the same answer: one branch per candidate
/// coordinate of each dynamic axis, every branch naming its element with
/// literals. The proven range is what makes the enumeration exhaustive, and
/// `prove_dynamic_index` has already refused any subscript that lacks one, so
/// the final candidate is the residual case and carries no condition of its own.
///
/// The value is bound to one local ahead of the branches: it is then evaluated
/// once, at the write's own position in the statement order, and the emitted
/// size stays linear in the candidate count rather than multiplying by it.
fn lower_element_write(
    lowerer: &mut ExpressionLowerer<'_, '_>,
    write: ElementWrite<'_>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let dynamic = write
        .subscripts
        .iter()
        .enumerate()
        .filter(|(_, index)| !lowerer.is_loop_index_expression(index))
        .map(|(axis, _)| axis)
        .collect::<Vec<_>>();
    if dynamic.is_empty() {
        return Ok(vec![element_assignment(
            &write,
            write.subscripts.to_vec(),
            write.value.clone(),
        )]);
    }
    let candidates = write_candidates(&dynamic, write.dimensions, write.span)?;
    let Some((residual, guarded)) = candidates.split_last() else {
        unreachable!("a dynamic axis contributes at least one candidate coordinate")
    };
    if guarded.is_empty() {
        // A single candidate names the element outright, so the selection is
        // the literal subscript list and needs neither a branch nor a binding.
        return Ok(vec![element_assignment(
            &write,
            candidate_subscripts(&write, &dynamic, residual),
            write.value.clone(),
        )]);
    }
    let (binding, value) = lowerer.bind_element_write_value(&write);
    let branches = guarded
        .iter()
        .map(|candidate| gast::IfBranch {
            condition: gast::Condition::Expression(candidate_condition(
                &write, &dynamic, candidate,
            )),
            body: vec![element_assignment(
                &write,
                candidate_subscripts(&write, &dynamic, candidate),
                value.clone(),
            )],
            span: write.span,
        })
        .collect();
    Ok(vec![
        binding,
        gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches,
                else_body: Some(vec![element_assignment(
                    &write,
                    candidate_subscripts(&write, &dynamic, residual),
                    value,
                )]),
            }),
            write.span,
        ),
    ])
}

impl ExpressionLowerer<'_, '_> {
    /// Bind one element write's value to a local, and return the binding
    /// statement together with the reference the branches store from.
    ///
    /// The binding is returned rather than queued as a prefix statement so it
    /// stays at the write's own position: a chain of writes has its prefixes
    /// hoisted ahead of all of them, which would compute a later write's value
    /// before an earlier write has stored.
    fn bind_element_write_value(
        &mut self,
        write: &ElementWrite<'_>,
    ) -> (gast::Spanned<gast::Statement>, gast::Expression) {
        let name = gast::Name::ident(format!(
            "rumoca_{}_element_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(write.scalar),
            name: name.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span: write.span,
        });
        let binding = gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(name.clone()),
                value: write.value.clone(),
            },
            write.span,
        );
        (binding, gast::Expression::Ref(gast::Reference::local(name)))
    }
}

fn element_assignment(
    write: &ElementWrite<'_>,
    subscripts: Vec<gast::Expression>,
    value: gast::Expression,
) -> gast::Spanned<gast::Statement> {
    gast::Spanned::new(
        gast::Statement::Assignment {
            target: gast::Reference::Local(gast::RefPart {
                name: write.target.clone(),
                subscripts,
                span: write.span,
            }),
            value,
        },
        write.span,
    )
}

/// Every coordinate the dynamic axes of one write can name, in row-major order.
fn write_candidates(
    dynamic: &[usize],
    dimensions: &[u32],
    span: Span,
) -> Result<Vec<Vec<i64>>, GalecTargetError> {
    let extents = dynamic
        .iter()
        .map(|axis| dimensions[*axis])
        .collect::<Vec<_>>();
    let count = extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        .filter(|count| *count > 0 && *count <= MAX_WRITE_CANDIDATES)
        .ok_or_else(|| {
            unsupported(
                "dynamic-array-element-write",
                format!(
                    "a dynamic element write spans more than {MAX_WRITE_CANDIDATES} candidate coordinates"
                ),
                span,
            )
        })?;
    let mut candidates = Vec::with_capacity(count);
    let mut current = vec![1i64; extents.len()];
    loop {
        candidates.push(current.clone());
        let Some(axis) = (0..extents.len())
            .rev()
            .find(|axis| current[*axis] < i64::from(extents[*axis]))
        else {
            return Ok(candidates);
        };
        current[axis] += 1;
        current[axis + 1..].fill(1);
    }
}

/// The subscript list one candidate names: literals on the dynamic axes, the
/// written subscript everywhere else.
fn candidate_subscripts(
    write: &ElementWrite<'_>,
    dynamic: &[usize],
    candidate: &[i64],
) -> Vec<gast::Expression> {
    let mut subscripts = write.subscripts.to_vec();
    for (axis, coordinate) in dynamic.iter().zip(candidate) {
        subscripts[*axis] = gast::Expression::Integer(*coordinate);
    }
    subscripts
}

/// The test that selects one candidate: every dynamic subscript equals the
/// coordinate the candidate names on its axis.
fn candidate_condition(
    write: &ElementWrite<'_>,
    dynamic: &[usize],
    candidate: &[i64],
) -> gast::Expression {
    dynamic
        .iter()
        .zip(candidate)
        .map(|(axis, coordinate)| {
            gast::Expression::binary(
                gast::BinaryOp::Eq,
                write.subscripts[*axis].clone(),
                gast::Expression::Integer(*coordinate),
            )
        })
        .reduce(|lhs, rhs| gast::Expression::binary(gast::BinaryOp::And, lhs, rhs))
        .expect("an expanded element write has at least one dynamic axis")
}
