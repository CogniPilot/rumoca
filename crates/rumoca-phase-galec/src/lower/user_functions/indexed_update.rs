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
    if chain.writes.is_empty() || !writes_may_run_in_place(view, target, chain.root) {
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
