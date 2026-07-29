use super::*;
use crate::expression::{OperandRange, PackedSubscriptKind};

pub(super) fn expect_complete_function(
    storage: &Storage,
    function: FunctionId<'_>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let entry = storage
        .functions
        .get(function.index() as usize)
        .ok_or_else(|| unknown("function", function.index(), provenance))?;
    if entry.definition.is_some() {
        return Ok(());
    }
    Err(DaeConstructionError::IncompleteDefinition {
        kind: "function",
        index: function.index(),
        span: provenance.span(),
    })
}

pub(super) fn validate_function_dependencies(
    storage: &Storage,
    body: &FunctionBody<'_>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let limit = match body.construction {
        FunctionConstruction::Acyclic => body.function.index(),
        FunctionConstruction::Recursive { start, end } => {
            if !(start..end).contains(&body.function.index()) {
                return Err(DaeConstructionError::InvalidRecursiveFunctionGroup {
                    span: provenance.span(),
                });
            }
            end
        }
    };
    let function = &storage.functions[body.function.index() as usize];
    for definition in &function.definitions {
        let latest_call = storage
            .expressions
            .function_latest_calls
            .get(definition.rhs as usize)
            .copied()
            .ok_or_else(|| unknown("expression", definition.rhs, provenance))?;
        let Some(call) = latest_call else {
            continue;
        };
        if call.target < limit {
            continue;
        }
        let span = storage
            .expressions
            .provenance
            .get(call.witness as usize)
            .copied()
            .ok_or_else(|| unknown("expression provenance", call.witness, provenance))?
            .span();
        return Err(DaeConstructionError::InvalidFunctionDependency {
            function: body.function.index(),
            target: call.target,
            span,
        });
    }
    Ok(())
}

pub(super) fn validate_recursive_function_group(
    storage: &Storage,
    functions: &[FunctionId<'_>],
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let start = functions[0].index();
    let end = start + functions.len() as u32;
    let mut edges = vec![Vec::new(); functions.len()];
    for (ordinal, function) in functions.iter().enumerate() {
        if function.index() != start + ordinal as u32 {
            return Err(DaeConstructionError::InvalidRecursiveFunctionGroup {
                span: provenance.span(),
            });
        }
        collect_group_dependencies(storage, *function, start, end, &mut edges[ordinal])?;
    }
    let components = rumoca_core::dependency_first_sccs(&edges)
        .expect("checked recursive calls reference their reserved group");
    if matches!(components.as_slice(), [component] if component.recursive) {
        return Ok(());
    }
    Err(DaeConstructionError::InvalidRecursiveFunctionGroup {
        span: provenance.span(),
    })
}

fn collect_group_dependencies(
    storage: &Storage,
    function: FunctionId<'_>,
    group_start: u32,
    group_end: u32,
    dependencies: &mut Vec<usize>,
) -> Result<(), DaeConstructionError> {
    let entry = &storage.functions[function.index() as usize];
    let mut seen = vec![false; storage.expressions.nodes.len()];
    let mut pending = entry
        .definitions
        .iter()
        .map(|definition| definition.rhs)
        .collect::<Vec<_>>();
    while let Some(expression) = pending.pop() {
        let index = expression as usize;
        if std::mem::replace(&mut seen[index], true) {
            continue;
        }
        match &storage.expressions.nodes[index] {
            ExprNode::Call {
                function, operands, ..
            } => {
                record_group_dependency(*function, group_start, group_end, dependencies);
                push_operands(storage, *operands, &mut pending);
            }
            node => push_expression_children(storage, node, &mut pending),
        }
    }
    dependencies.sort_unstable();
    Ok(())
}

fn record_group_dependency(
    function: u32,
    group_start: u32,
    group_end: u32,
    dependencies: &mut Vec<usize>,
) {
    if !(group_start..group_end).contains(&function) {
        return;
    }
    let ordinal = (function - group_start) as usize;
    if !dependencies.contains(&ordinal) {
        dependencies.push(ordinal);
    }
}

fn push_expression_children(storage: &Storage, node: &ExprNode, pending: &mut Vec<u32>) {
    match node {
        ExprNode::Unary { operand, .. }
        | ExprNode::Field { base: operand, .. }
        | ExprNode::Comprehension { body: operand, .. } => pending.push(*operand),
        ExprNode::Binary { lhs, rhs, .. } => pending.extend([*lhs, *rhs]),
        ExprNode::Index { base, subscripts } => {
            pending.push(*base);
            push_subscripts(storage, *subscripts, pending);
        }
        ExprNode::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            pending.extend([*base, *value]);
            push_subscripts(storage, *subscripts, pending);
        }
        ExprNode::Conditional { operands }
        | ExprNode::Array { operands }
        | ExprNode::Record { operands }
        | ExprNode::Builtin { operands, .. }
        | ExprNode::Call { operands, .. } => push_operands(storage, *operands, pending),
        ExprNode::Literal(_)
        | ExprNode::Coordinate(_)
        | ExprNode::Range { .. }
        | ExprNode::FunctionValue { .. }
        | ExprNode::FunctionFoldParameter { .. }
        | ExprNode::FunctionFoldOutput { .. } => {}
    }
}

fn push_operands(storage: &Storage, operands: OperandRange, pending: &mut Vec<u32>) {
    pending.extend(
        storage.expressions.operands[operands.indices()]
            .iter()
            .copied(),
    );
}

fn push_subscripts(storage: &Storage, subscripts: OperandRange, pending: &mut Vec<u32>) {
    for subscript in &storage.expressions.subscripts[subscripts.indices()] {
        match subscript.kind {
            PackedSubscriptKind::Index(expression) | PackedSubscriptKind::Slice(expression) => {
                pending.push(expression);
            }
            PackedSubscriptKind::Whole => {}
        }
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
        parameter_definitions: Vec::new(),
        initial_definitions: initial_values,
        update_definitions: Vec::new(),
        output_definitions: Vec::new(),
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

pub(super) fn function_definition_entry<'storage>(
    storage: &'storage Storage,
    definition: FunctionDefinitionId<'_>,
    provenance: DaeProvenance,
) -> Result<&'storage FunctionDefinitionEntry, DaeConstructionError> {
    storage
        .functions
        .get(definition.function().index() as usize)
        .and_then(|function| function.definitions.get(definition.ordinal() as usize))
        .ok_or_else(|| unknown("function definition", definition.ordinal(), provenance))
}

pub(super) fn insert_function_definition<'dae>(
    storage: &mut Storage,
    target: FunctionValueId<'dae>,
    rhs: ExprId<'dae>,
    provenance: DaeProvenance,
) -> Result<FunctionDefinitionId<'dae>, DaeConstructionError> {
    let function = storage
        .functions
        .get_mut(target.function().index() as usize)
        .ok_or_else(|| unknown("function", target.function().index(), provenance))?;
    let ordinal = checked_u32(
        function.definitions.len(),
        "function definition arena",
        provenance,
    )?;
    function.definitions.push(FunctionDefinitionEntry {
        target: target.ordinal(),
        rhs: rhs.index(),
        provenance,
    });
    Ok(FunctionDefinitionId::from_raw(
        target.function().index(),
        ordinal,
    ))
}

pub(super) fn next_function_definition_id<'dae>(
    storage: &Storage,
    function: FunctionId<'dae>,
    provenance: DaeProvenance,
) -> Result<FunctionDefinitionId<'dae>, DaeConstructionError> {
    let definitions = &storage
        .functions
        .get(function.index() as usize)
        .ok_or_else(|| unknown("function", function.index(), provenance))?
        .definitions;
    let ordinal = checked_u32(definitions.len(), "function definition arena", provenance)?;
    Ok(FunctionDefinitionId::from_raw(function.index(), ordinal))
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
    if let Some(raw) = storage
        .expressions
        .function_illegal_coordinates
        .get(expression.index() as usize)
        .copied()
        .ok_or_else(|| unknown("expression", expression.index(), provenance))?
    {
        let node = storage
            .expressions
            .nodes
            .get(raw as usize)
            .ok_or_else(|| unknown("expression", raw, provenance))?;
        let coordinate_provenance = storage
            .expressions
            .provenance
            .get(raw as usize)
            .copied()
            .ok_or_else(|| unknown("expression provenance", raw, provenance))?;
        reject_model_coordinate(node, coordinate_provenance)?;
    }
    let reads = storage
        .expressions
        .function_read_sets
        .get(expression.index() as usize)
        .copied()
        .ok_or_else(|| unknown("expression", expression.index(), provenance))?;
    storage.function_read_sets.try_for_each(reads, &mut |fact| {
        let expected = body
            .current_values
            .get(fact.value as usize)
            .copied()
            .flatten();
        if expected == Some(fact.definition) {
            return Ok(());
        }
        let span = storage
            .expressions
            .provenance
            .get(fact.witness as usize)
            .copied()
            .ok_or_else(|| unknown("expression provenance", fact.witness, provenance))?
            .span();
        Err(DaeConstructionError::InvalidFunctionValueRead {
            value: fact.value,
            expected_definition: expected,
            found_definition: fact.definition,
            span,
        })
    })
}

fn reject_model_coordinate(
    node: &ExprNode,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let coordinate = match node {
        ExprNode::Coordinate(Coordinate::FunctionParameter { .. } | Coordinate::Binder { .. })
        | ExprNode::Literal(_)
        | ExprNode::Range { .. }
        | ExprNode::FunctionValue { .. }
        | ExprNode::FunctionFoldParameter { .. }
        | ExprNode::FunctionFoldOutput { .. }
        | ExprNode::Unary { .. }
        | ExprNode::Binary { .. }
        | ExprNode::Field { .. }
        | ExprNode::Comprehension { .. }
        | ExprNode::Index { .. }
        | ExprNode::ArrayUpdate { .. }
        | ExprNode::Conditional { .. }
        | ExprNode::Array { .. }
        | ExprNode::Record { .. }
        | ExprNode::Builtin { .. }
        | ExprNode::Call { .. } => return Ok(()),
        ExprNode::Coordinate(Coordinate::Parameter(_)) => "parameter",
        ExprNode::Coordinate(Coordinate::Input(_)) => "input",
        ExprNode::Coordinate(Coordinate::State(_)) => "state",
        ExprNode::Coordinate(Coordinate::Derivative(_)) => "derivative",
        ExprNode::Coordinate(Coordinate::Algebraic(_)) => "algebraic",
        ExprNode::Coordinate(Coordinate::DiscreteReal(_)) => "discrete real",
        ExprNode::Coordinate(Coordinate::DiscreteValue(_)) => "discrete value",
        ExprNode::Coordinate(Coordinate::PreDiscreteReal(_)) => "pre(discrete real)",
        ExprNode::Coordinate(Coordinate::PreDiscreteValue(_)) => "pre(discrete value)",
        ExprNode::Coordinate(Coordinate::Time) => "time",
        ExprNode::Coordinate(Coordinate::Condition(_)) => "condition",
        ExprNode::Coordinate(Coordinate::Delay(_)) => "delay",
        ExprNode::Coordinate(Coordinate::Previous(_)) => "previous",
        ExprNode::Coordinate(Coordinate::Terminal(_)) => "terminal",
    };
    Err(DaeConstructionError::InvalidFunctionCoordinate {
        coordinate,
        span: provenance.span(),
    })
}
