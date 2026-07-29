use super::*;

pub(in crate::construction) enum ModelAlgorithmPlan {
    Declarative {
        target: VarName,
    },
    TotalArrayDefinition {
        target: VarName,
        domain: StructuredIndexDomain,
        binder_spans: Vec<Span>,
    },
    SeparatedArraySum {
        array_target: VarName,
        scalar_target: VarName,
        domain: StructuredIndexDomain,
        binder_spans: Vec<Span>,
    },
    Event,
}

pub(super) fn analyze_model_algorithm(
    flat: &flat::Model,
    algorithm: &flat::Algorithm,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<ModelAlgorithmPlan, ToDaeError> {
    if contains_when(&algorithm.statements) {
        let targets = model_algorithm_targets(flat, algorithm);
        if targets.iter().any(|target| {
            !matches!(
                roles[target],
                PlannedRole::DiscreteReal | PlannedRole::DiscreteValue
            )
        }) {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                "a mixed continuous/event algorithm requires one checked atomic owner",
                algorithm.span,
            ));
        }
        return Ok(ModelAlgorithmPlan::Event);
    }
    let targets = model_algorithm_targets(flat, algorithm);
    if let Some(plan) = analyze_separated_array_sum(flat, algorithm, &targets)? {
        return Ok(plan);
    }
    let [target] = targets.as_slice() else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "a multi-output algorithm requires one checked atomic vector-equation owner",
            algorithm.span,
        ));
    };
    let variable = &flat.variables[target];
    if !variable.dims.is_empty() {
        return analyze_total_array_definition(algorithm, target, &variable.dims);
    }
    if !matches!(
        roles[target],
        PlannedRole::Algebraic
            | PlannedRole::Output
            | PlannedRole::DiscreteReal
            | PlannedRole::DiscreteValue
    ) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "algorithm target `{target}` has non-computable role {:?}",
                roles[target]
            ),
            algorithm.span,
        ));
    }
    let assigned =
        validate_declarative_sequence(&algorithm.statements, target, false, algorithm.span)?;
    if !assigned {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!("algorithm does not define `{target}` on every control-flow path"),
            algorithm.span,
        ));
    }
    Ok(ModelAlgorithmPlan::Declarative {
        target: target.clone(),
    })
}

fn analyze_separated_array_sum(
    flat: &flat::Model,
    algorithm: &flat::Algorithm,
    targets: &[VarName],
) -> Result<Option<ModelAlgorithmPlan>, ToDaeError> {
    let [first, second] = targets else {
        return Ok(None);
    };
    let (array_target, scalar_target) = match (
        flat.variables[first].dims.is_empty(),
        flat.variables[second].dims.is_empty(),
    ) {
        (false, true) => (first, second),
        (true, false) => (second, first),
        _ => return Ok(None),
    };
    let [
        rumoca_core::Statement::Assignment {
            comp: initial_target,
            value: initial,
            ..
        },
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        },
    ] = algorithm.statements.as_slice()
    else {
        return Ok(None);
    };
    let [
        rumoca_core::Statement::Assignment {
            comp: array_component,
            value: element,
            ..
        },
        rumoca_core::Statement::Assignment {
            comp: update_target,
            value: update,
            ..
        },
    ] = equations.as_slice()
    else {
        return Ok(None);
    };
    if assignment_target(initial_target) != *scalar_target
        || !is_zero(initial)
        || assignment_target(array_component) != *array_target
        || assignment_target(update_target) != *scalar_target
    {
        return Ok(None);
    }
    let Some(subscripts) = array_component.parts.last().map(|part| part.subs.as_slice()) else {
        return Ok(None);
    };
    let dimensions = &flat.variables[array_target].dims;
    if indices.len() != dimensions.len() || subscripts.len() != dimensions.len() {
        return Ok(None);
    }
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for (ordinal, ((index, subscript), extent)) in indices
        .iter()
        .zip(subscripts)
        .zip(dimensions)
        .enumerate()
    {
        validate_total_axis(index, subscript, *extent)?;
        let range_span = expression_span(&index.range)?;
        binders.push(StructuredIndexBinder {
            id: ordinal,
            display_name: index.ident.clone(),
            lower: 1,
            upper: *extent,
            step: 1,
        });
        binder_spans.push(range_span);
    }
    reject_read_before_definition(element, array_target, false)?;
    reject_read_before_definition(element, scalar_target, false)?;
    if !is_additive_element_update(update, scalar_target, array_target, subscripts) {
        return Ok(None);
    }
    let domain = StructuredIndexDomain { binders };
    domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_algorithm(
            "model",
            format!("separated array-reduction domain is not computable: {error}"),
            *span,
        )
    })?;
    Ok(Some(ModelAlgorithmPlan::SeparatedArraySum {
        array_target: array_target.clone(),
        scalar_target: scalar_target.clone(),
        domain,
        binder_spans,
    }))
}

fn is_zero(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::Literal {
            value: Literal::Integer(0) | Literal::Real(0.0),
            ..
        }
    )
}

fn is_additive_element_update(
    expression: &Expression,
    scalar_target: &VarName,
    array_target: &VarName,
    expected_subscripts: &[Subscript],
) -> bool {
    let Expression::Binary {
        op: OpBinary::Add | OpBinary::AddElem,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return false;
    };
    is_unsubscripted_reference(lhs, scalar_target)
        && is_exact_element_reference(rhs, array_target, expected_subscripts)
}

fn is_unsubscripted_reference(expression: &Expression, target: &VarName) -> bool {
    matches!(
        expression,
        Expression::VarRef {
            name, subscripts, ..
        } if name.var_name() == target && subscripts.is_empty()
    )
}

fn is_exact_element_reference(
    expression: &Expression,
    target: &VarName,
    expected_subscripts: &[Subscript],
) -> bool {
    matches!(
        expression,
        Expression::VarRef {
            name, subscripts, ..
        } if name.var_name() == target && subscripts == expected_subscripts
    )
}

fn analyze_total_array_definition(
    algorithm: &flat::Algorithm,
    target: &VarName,
    dimensions: &[i64],
) -> Result<ModelAlgorithmPlan, ToDaeError> {
    let [
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        },
    ] = algorithm.statements.as_slice()
    else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "an array algorithm requires one compact total-definition loop",
            algorithm.span,
        ));
    };
    let [rumoca_core::Statement::Assignment { comp, value, .. }] = equations.as_slice() else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "a total array-definition loop requires one element assignment",
            *span,
        ));
    };
    let Some(component) = comp.parts.last() else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "array loop assignment has no checked target",
            *span,
        ));
    };
    if assignment_target(comp) != *target
        || indices.len() != dimensions.len()
        || component.subs.len() != dimensions.len()
    {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "array loop must bind every target axis exactly once",
            *span,
        ));
    }
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for (ordinal, ((index, subscript), extent)) in indices
        .iter()
        .zip(&component.subs)
        .zip(dimensions)
        .enumerate()
    {
        validate_total_axis(index, subscript, *extent)?;
        let range_span = expression_span(&index.range)?;
        binders.push(StructuredIndexBinder {
            id: ordinal,
            display_name: index.ident.clone(),
            lower: 1,
            upper: *extent,
            step: 1,
        });
        binder_spans.push(range_span);
    }
    reject_read_before_definition(value, target, false)?;
    let domain = StructuredIndexDomain { binders };
    domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_algorithm(
            "model",
            format!("array loop domain is not computable: {error}"),
            *span,
        )
    })?;
    Ok(ModelAlgorithmPlan::TotalArrayDefinition {
        target: target.clone(),
        domain,
        binder_spans,
    })
}

fn validate_total_axis(
    index: &rumoca_core::ForIndex,
    subscript: &Subscript,
    extent: i64,
) -> Result<(), ToDaeError> {
    let span = expression_span(&index.range)?;
    let Expression::Range {
        start, step, end, ..
    } = &index.range
    else {
        return Err(invalid_total_axis(
            index,
            "axis requires an explicit range",
            span,
        ));
    };
    let exact_range = integer_value(start) == Some(1)
        && step.as_deref().map(integer_value).unwrap_or(Some(1)) == Some(1)
        && integer_value(end) == Some(extent);
    let exact_subscript = matches!(
        subscript,
        Subscript::Expr { expr, .. }
            if matches!(
                expr.as_ref(),
                Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == index.ident && subscripts.is_empty()
            )
    );
    if exact_range && exact_subscript && extent >= 0 {
        Ok(())
    } else {
        Err(invalid_total_axis(
            index,
            "range and subscript must cover one declared array axis exactly",
            span,
        ))
    }
}

fn invalid_total_axis(index: &rumoca_core::ForIndex, detail: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_algorithm(
        "model",
        format!("loop index `{}`: {detail}", index.ident),
        span,
    )
}

fn integer_value(expression: &Expression) -> Option<i64> {
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

fn validate_declarative_sequence(
    statements: &[rumoca_core::Statement],
    target: &VarName,
    mut assigned: bool,
    owner_span: Span,
) -> Result<bool, ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                let written = assignment_target(comp);
                if &written != target || comp.parts.iter().any(|part| !part.subs.is_empty()) {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "declarative scalar algorithm assignment escaped its checked target",
                        *span,
                    ));
                }
                reject_read_before_definition(value, target, assigned)?;
                assigned = true;
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                let mut exits = Vec::with_capacity(cond_blocks.len() + 1);
                for block in cond_blocks {
                    reject_read_before_definition(&block.cond, target, assigned)?;
                    exits.push(validate_declarative_sequence(
                        &block.stmts,
                        target,
                        assigned,
                        owner_span,
                    )?);
                }
                exits.push(match else_block {
                    Some(fallback) => {
                        validate_declarative_sequence(fallback, target, assigned, owner_span)?
                    }
                    None => assigned,
                });
                assigned = exits.into_iter().all(std::convert::identity);
            }
            _ => {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "declarative algorithm requires scalar assignments and conditionals",
                    statement.source_span().unwrap_or(owner_span),
                ));
            }
        }
    }
    Ok(assigned)
}

fn reject_read_before_definition(
    expression: &Expression,
    target: &VarName,
    assigned: bool,
) -> Result<(), ToDaeError> {
    if assigned {
        return Ok(());
    }
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    if references.iter().any(|reference| reference == target) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "`{target}` is read before definition; checked start/pre initialization is required"
            ),
            expression_span(expression)?,
        ));
    }
    Ok(())
}

pub(in crate::construction) fn event_targets(flat: &flat::Model) -> HashSet<VarName> {
    let mut written = when_clause_targets(flat);
    for algorithm in &flat.algorithms {
        collect_nested_when_targets(&algorithm.statements, &mut written);
    }
    resolve_written_targets(flat, written)
}

pub(in crate::construction) fn when_clause_targets(flat: &flat::Model) -> HashSet<VarName> {
    let mut written = HashSet::new();
    for clause in &flat.when_clauses {
        collect_when_equation_targets(&clause.equations, &mut written);
    }
    resolve_written_targets(flat, written)
}

pub(in crate::construction) fn algorithm_targets(flat: &flat::Model) -> HashSet<VarName> {
    flat.algorithms
        .iter()
        .flat_map(|algorithm| model_algorithm_targets(flat, algorithm))
        .collect()
}

pub(in crate::construction) fn model_algorithm_targets(
    flat: &flat::Model,
    algorithm: &flat::Algorithm,
) -> Vec<VarName> {
    let mut written = HashSet::new();
    collect_statement_targets(&algorithm.statements, &mut written);
    let mut targets = resolve_written_targets(flat, written)
        .into_iter()
        .collect::<Vec<_>>();
    targets.sort_by(|left, right| left.as_str().cmp(right.as_str()));
    targets
}

fn collect_statement_targets(
    statements: &[rumoca_core::Statement],
    targets: &mut HashSet<VarName>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } if !comp.parts.is_empty() => {
                targets.insert(assignment_target(comp));
            }
            rumoca_core::Statement::FunctionCall { outputs, .. } => {
                targets.extend(outputs.iter().flatten().map(|output| output.to_var_name()));
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_statement_targets(equations, targets);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_statement_targets(&block.stmts, targets);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_statement_targets(&block.stmts, targets);
                }
                if let Some(fallback) = else_block {
                    collect_statement_targets(fallback, targets);
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_statement_targets(&block.stmts, targets);
                }
            }
            _ => {}
        }
    }
}

fn contains_when(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::When { .. } => true,
        rumoca_core::Statement::For { equations, .. } => contains_when(equations),
        rumoca_core::Statement::While { block, .. } => contains_when(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| contains_when(&block.stmts))
                || else_block.as_deref().is_some_and(contains_when)
        }
        _ => false,
    })
}

fn collect_nested_when_targets(
    statements: &[rumoca_core::Statement],
    targets: &mut HashSet<VarName>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_statement_targets(&block.stmts, targets);
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_nested_when_targets(equations, targets);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_nested_when_targets(&block.stmts, targets);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_nested_when_targets(&block.stmts, targets);
                }
                if let Some(fallback) = else_block {
                    collect_nested_when_targets(fallback, targets);
                }
            }
            _ => {}
        }
    }
}

fn collect_when_equation_targets(equations: &[flat::WhenEquation], targets: &mut HashSet<VarName>) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, .. } => {
                targets.insert(target.clone());
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, equations) in branches {
                    collect_when_equation_targets(equations, targets);
                }
                collect_when_equation_targets(else_branch, targets);
            }
            flat::WhenEquation::FunctionCallOutputs { outputs, .. } => {
                targets.extend(outputs.iter().cloned());
            }
            flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. } => {}
        }
    }
}

fn resolve_written_targets(flat: &flat::Model, written: HashSet<VarName>) -> HashSet<VarName> {
    let mut targets = HashSet::new();
    for target in written {
        if flat.variables.contains_key(&target) {
            targets.insert(target);
            continue;
        }
        let prefix = format!("{target}.");
        targets.extend(
            flat.variables
                .keys()
                .filter(|name| name.as_str().starts_with(&prefix))
                .cloned(),
        );
    }
    targets
}

fn assignment_target(component: &rumoca_core::ComponentReference) -> VarName {
    rumoca_core::component_ref_to_base_reference(component)
        .var_name()
        .clone()
}
