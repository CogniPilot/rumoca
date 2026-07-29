use super::*;
use crate::expression::OperandRange;

impl<'dae> FunctionLoop<'dae> {
    pub const fn fold(&self) -> FunctionFoldId<'dae> {
        self.fold
    }

    pub const fn body(&self) -> &FunctionBody<'dae> {
        &self.body
    }
}

pub(super) fn expect_function_body_expression(
    storage: &Storage,
    body: &FunctionBody<'_>,
    expression: ExprId<'_>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    match body.domain {
        Some(domain) => storage.expect_domain_expression(expression, domain, provenance)?,
        None => {
            if let Some(found_domain) = storage.expr_binder_domain(expression, provenance)? {
                return Err(DaeConstructionError::InvalidBinderScope {
                    expected_domain: None,
                    found_domain,
                    span: provenance.span(),
                });
            }
        }
    }
    match storage.expr_function_scope(expression, provenance)? {
        None => Ok(()),
        Some(function) if function == body.function.index() => Ok(()),
        Some(function) => Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(body.function.index()),
            found_function: function,
            span: provenance.span(),
        }),
    }
}

pub(super) fn function_fold_raw(
    storage: &Storage,
    fold: FunctionFoldId<'_>,
    provenance: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    let raw = storage
        .functions
        .get(fold.function().index() as usize)
        .and_then(|function| function.folds.get(fold.ordinal() as usize))
        .copied()
        .ok_or_else(|| unknown("function fold", fold.ordinal(), provenance))?;
    let entry = storage
        .function_folds
        .get(raw as usize)
        .ok_or_else(|| unknown("function fold", raw, provenance))?;
    if entry.function != fold.function().index() || entry.ordinal != fold.ordinal() {
        return Err(DaeConstructionError::UnknownId {
            kind: "function fold",
            index: fold.ordinal(),
            span: provenance.span(),
        });
    }
    Ok(raw)
}

pub(super) fn expect_expression_function_scope(
    storage: &Storage,
    expression: ExprId<'_>,
    function: FunctionId<'_>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    match storage.expr_function_scope(expression, provenance)? {
        None => Ok(()),
        Some(found) if found == function.index() => Ok(()),
        Some(found) => Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(function.index()),
            found_function: found,
            span: provenance.span(),
        }),
    }
}

pub(super) fn expect_function_loop_generation(
    entry: &FunctionFoldEntry,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let expected =
        DaeProvenance::generated(DaeGeneration::FunctionLoopLowering, entry.provenance.span())?;
    if provenance == expected {
        return Ok(());
    }
    Err(DaeConstructionError::MalformedWire {
        column: "function_folds.provenance",
    })
}

pub(super) fn reserve_function_fold<'dae>(
    storage: &mut Storage,
    function: FunctionId<'dae>,
    domain: DomainId<'dae>,
    targets: Vec<u32>,
    initial_values: Vec<u32>,
    provenance: DaeProvenance,
) -> Result<FunctionFoldId<'dae>, DaeConstructionError> {
    let ordinal = storage
        .functions
        .get(function.index() as usize)
        .map(|function| function.folds.len())
        .ok_or_else(|| unknown("function", function.index(), provenance))?;
    let ordinal = checked_u32(ordinal, "function fold", provenance)?;
    let raw = checked_u32(
        storage.function_folds.len(),
        "function fold arena",
        provenance,
    )?;
    storage.functions[function.index() as usize].folds.push(raw);
    storage.function_folds.push(FunctionFoldEntry {
        function: function.index(),
        ordinal,
        domain: domain.index(),
        targets,
        parameter_values: Vec::new(),
        initial_values,
        update_values: Vec::new(),
        output_values: Vec::new(),
        provenance,
    });
    storage.unfilled_function_folds += 1;
    Ok(FunctionFoldId::from_raw(function.index(), ordinal))
}

pub(super) fn ensure_unique_function_name(
    function: &FunctionEntry,
    name: &VarName,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if function
        .parameter_values
        .iter()
        .any(|value| &value.name == name)
        || function.values.iter().any(|value| &value.name == name)
    {
        return Err(DaeConstructionError::DuplicateKey {
            kind: "function value",
            key: name.to_string(),
            span: provenance.span(),
        });
    }
    Ok(())
}

pub(super) fn function_value_entry<'storage>(
    storage: &'storage Storage,
    value: FunctionValueId<'_>,
    provenance: DaeProvenance,
) -> Result<&'storage FunctionValueEntry, DaeConstructionError> {
    storage
        .functions
        .get(value.function().index() as usize)
        .and_then(|function| function.values.get(value.ordinal() as usize))
        .ok_or_else(|| unknown("function value", value.ordinal(), provenance))
}

pub(super) fn check_function_value_owner<'dae>(
    function: FunctionId<'dae>,
    value: FunctionValueId<'dae>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if value.function() == function {
        return Ok(());
    }
    Err(DaeConstructionError::InvalidFunctionScope {
        expected_function: Some(function.index()),
        found_function: value.function().index(),
        span: provenance.span(),
    })
}

pub(super) fn validate_function_value_reads(
    storage: &Storage,
    body: &FunctionBody<'_>,
    expression: ExprId<'_>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let mut pending = vec![expression.index()];
    let mut visited = vec![false; storage.expressions.nodes.len()];
    while let Some(raw) = pending.pop() {
        let node = storage
            .expressions
            .nodes
            .get(raw as usize)
            .ok_or_else(|| unknown("expression", raw, provenance))?;
        if std::mem::replace(&mut visited[raw as usize], true) {
            continue;
        }
        push_function_expression_children(storage, node, &mut pending, provenance)?;
        if let ExprNode::FunctionValue {
            function,
            value,
            definition,
        } = node
        {
            if *function != body.function.index() {
                return Err(DaeConstructionError::InvalidFunctionScope {
                    expected_function: Some(body.function.index()),
                    found_function: *function,
                    span: provenance.span(),
                });
            }
            let expected = body.current_values.get(*value as usize).copied().flatten();
            if expected != Some(*definition) {
                return Err(DaeConstructionError::InvalidFunctionValueRead {
                    value: *value,
                    expected_definition: expected,
                    found_definition: *definition,
                    span: provenance.span(),
                });
            }
        }
    }
    Ok(())
}

fn push_function_expression_children(
    storage: &Storage,
    node: &ExprNode,
    pending: &mut Vec<u32>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    match node {
        ExprNode::Literal(_)
        | ExprNode::Coordinate(_)
        | ExprNode::Range { .. }
        | ExprNode::FunctionFoldParameter { .. }
        | ExprNode::FunctionFoldOutput { .. } => {}
        ExprNode::Unary { operand, .. } => pending.push(*operand),
        ExprNode::Binary { lhs, rhs, .. } => pending.extend([*lhs, *rhs]),
        ExprNode::Field { base, .. } => pending.push(*base),
        ExprNode::Comprehension { body, .. } => pending.push(*body),
        // A function-value use is an SSA environment boundary. Its attached
        // definition was validated when that use was inserted; traversing into
        // the immutable historical definition would incorrectly revalidate
        // earlier reads against the body's current environment.
        ExprNode::FunctionValue { .. } => {}
        ExprNode::Index { base, subscripts } => {
            pending.push(*base);
            push_subscript_children(storage, *subscripts, pending, provenance)?;
        }
        ExprNode::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            pending.extend([*base, *value]);
            push_subscript_children(storage, *subscripts, pending, provenance)?;
        }
        ExprNode::Conditional { operands }
        | ExprNode::Array { operands }
        | ExprNode::Record { operands }
        | ExprNode::Builtin { operands, .. }
        | ExprNode::Call { operands, .. } => pending.extend(
            storage
                .expressions
                .operands
                .get(operands.indices())
                .ok_or_else(|| unknown("operand range", operands.start, provenance))?,
        ),
    }
    Ok(())
}

fn push_subscript_children(
    storage: &Storage,
    subscripts: OperandRange,
    pending: &mut Vec<u32>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    for subscript in storage
        .expressions
        .subscripts
        .get(subscripts.indices())
        .ok_or_else(|| unknown("subscript range", subscripts.start, provenance))?
    {
        if let PackedSubscriptKind::Index(value) | PackedSubscriptKind::Slice(value) =
            subscript.kind
        {
            pending.push(value);
        }
    }
    Ok(())
}
