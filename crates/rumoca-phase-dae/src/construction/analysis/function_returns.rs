use super::*;
use rumoca_core::Reference;

pub(super) fn validate_guarded_function_return(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<Option<FunctionPlan>, ToDaeError> {
    if !contains_return(&function.body) {
        return Ok(None);
    }
    let Some((first, tail)) = function.body.split_first() else {
        unreachable!("a function containing return has a statement")
    };
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block: None,
        span,
    } = first
    else {
        return Ok(None);
    };
    if cond_blocks.is_empty()
        || tail.is_empty()
        || !cond_blocks.iter().all(|block| {
            matches!(
                block.stmts.last(),
                Some(rumoca_core::Statement::Return { .. })
            )
        })
    {
        return Ok(None);
    }
    require_span(*span, "function return conditional")?;

    let targets = function
        .outputs
        .iter()
        .map(|output| VarName::new(&output.name))
        .collect::<Vec<_>>();
    let mut branches = Vec::with_capacity(cond_blocks.len());
    for block in cond_blocks {
        validate_function_expression_with_roles(
            &block.cond,
            context.roles,
            context.flat,
            context.shapes,
        )?;
        let Some((rumoca_core::Statement::Return { span }, statements)) = block.stmts.split_last()
        else {
            return unsupported_return_shape(function, first);
        };
        require_span(*span, "function return statement")?;
        // A returning branch runs from the entry state, so it proves its own
        // definedness certificate independently of the non-returning path.
        let mut branch_definitions = FunctionDefinitions::new(function);
        let plans = validate_function_statements(statements, context, &mut branch_definitions)?;
        validate_return_definitions(function, statements, &plans, &targets, *span)?;
        branches.push(plans);
    }
    let mut definitions = FunctionDefinitions::new(function);
    let tail = validate_function_statements(tail, context, &mut definitions)?;
    if targets
        .iter()
        .any(|target| !sequence_defines_target(&tail, target))
    {
        return Err(ToDaeError::unsupported_flat(
            "function return",
            format!(
                "`{}` must define every output on its non-returning path",
                function.name
            ),
            *span,
        ));
    }
    Ok(Some(FunctionPlan::GuardedReturn {
        branches,
        tail,
        targets,
    }))
}

/// Convert non-leading early returns into lexical conditional continuations.
///
/// Each returning branch owns the statements before its `return`; the
/// non-returning path owns the remaining sequence exactly once. Returns nested
/// in loops remain unsupported because they require a distinct loop-exit
/// capability rather than a conditional value join.
pub(super) struct NormalizedFunctionReturns {
    pub(super) statements: Vec<rumoca_core::Statement>,
    pub(super) guards: Vec<GeneratedBooleanDefinition>,
    pub(super) has_returns: bool,
}

#[derive(Clone)]
pub(super) struct GeneratedBooleanDefinition {
    pub(super) target: VarName,
    pub(super) value: Expression,
    pub(super) span: Span,
}

pub(super) fn normalize_function_returns(
    statements: &[rumoca_core::Statement],
) -> Result<NormalizedFunctionReturns, ToDaeError> {
    let has_returns = contains_return(statements);
    let mut normalized = Vec::new();
    let mut guards = Vec::new();
    let mut active: Option<Expression> = None;
    for statement in statements {
        if let Some((cond_blocks, span)) = guarded_return(statement) {
            let return_condition = disjoin_conditions(cond_blocks, span);
            let guard_span = cond_blocks
                .iter()
                .filter_map(|block| block.stmts.last())
                .find_map(rumoca_core::Statement::source_span)
                .unwrap_or(span);
            let target = rumoca_core::function_return_guard_name(guard_span.start.0);
            let value = active.as_ref().map_or_else(
                || return_condition.clone(),
                |active| Expression::If {
                    branches: vec![(active.clone(), return_condition.clone())],
                    else_branch: Box::new(Expression::Literal {
                        value: Literal::Boolean(false),
                        span: guard_span,
                    }),
                    span: guard_span,
                },
            );
            normalized.push(rumoca_core::Statement::Empty { span: guard_span });
            guards.push(GeneratedBooleanDefinition {
                target: target.clone(),
                value,
                span: guard_span,
            });
            let guarded_blocks = cond_blocks
                .iter()
                .filter_map(|block| {
                    let statements = &block.stmts[..block.stmts.len() - 1];
                    (!statements.is_empty()).then(|| rumoca_core::StatementBlock {
                        cond: active.as_ref().map_or_else(
                            || block.cond.clone(),
                            |active| and_condition(active.clone(), block.cond.clone(), span),
                        ),
                        stmts: statements.to_vec(),
                    })
                })
                .collect::<Vec<_>>();
            if !guarded_blocks.is_empty() {
                normalized.push(rumoca_core::Statement::If {
                    cond_blocks: guarded_blocks,
                    else_block: None,
                    span,
                });
            }
            let not_returned = Expression::Unary {
                op: OpUnary::Not,
                rhs: Box::new(Expression::VarRef {
                    name: Reference::generated(target.as_str()),
                    subscripts: Vec::new(),
                    span: guard_span,
                }),
                span: guard_span,
            };
            active = Some(active.map_or(not_returned.clone(), |active| {
                and_condition(active, not_returned, guard_span)
            }));
            continue;
        }
        if contains_return(std::slice::from_ref(statement)) {
            let span = required_statement_span(statement, "nested function return")?;
            return Err(ToDaeError::unsupported_flat(
                "function return",
                "a non-guarded nested return requires a checked control-flow owner",
                span,
            ));
        }
        if let Some(expanded) = snapshot_loop_conditional(statement, active.as_ref(), &mut guards) {
            normalized.extend(expanded);
            continue;
        }
        normalized.push(active.as_ref().map_or_else(
            || statement.clone(),
            |active| {
                rumoca_core::Statement::If {
                    cond_blocks: vec![rumoca_core::StatementBlock {
                        cond: active.clone(),
                        stmts: vec![statement.clone()],
                    }],
                    else_block: None,
                    span: statement
                        .source_span()
                        .expect("return normalization preserves statement provenance"),
                }
            },
        ));
    }
    Ok(NormalizedFunctionReturns {
        statements: normalized,
        guards,
        has_returns,
    })
}

fn snapshot_loop_conditional(
    statement: &rumoca_core::Statement,
    active: Option<&Expression>,
    guards: &mut Vec<GeneratedBooleanDefinition>,
) -> Option<Vec<rumoca_core::Statement>> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    else {
        return None;
    };
    if !cond_blocks
        .iter()
        .any(|block| statements_contain_loop(&block.stmts))
        && !else_block.as_deref().is_some_and(statements_contain_loop)
    {
        return None;
    }
    let mut expanded = Vec::new();
    let mut remaining = active.cloned().unwrap_or(Expression::Literal {
        value: Literal::Boolean(true),
        span: *span,
    });
    let mut branch_guards = Vec::with_capacity(cond_blocks.len());
    for block in cond_blocks {
        let guard_span = expression_span(&block.cond).ok()?;
        let target = rumoca_core::function_branch_guard_name(guard_span.start.0);
        let value = Expression::If {
            branches: vec![(remaining.clone(), block.cond.clone())],
            else_branch: Box::new(Expression::Literal {
                value: Literal::Boolean(false),
                span: guard_span,
            }),
            span: guard_span,
        };
        expanded.push(rumoca_core::Statement::Empty { span: guard_span });
        guards.push(GeneratedBooleanDefinition {
            target: target.clone(),
            value,
            span: guard_span,
        });
        let guard = Expression::VarRef {
            name: Reference::generated(target.as_str()),
            subscripts: Vec::new(),
            span: guard_span,
        };
        remaining = and_condition(
            remaining,
            Expression::Unary {
                op: OpUnary::Not,
                rhs: Box::new(guard.clone()),
                span: guard_span,
            },
            guard_span,
        );
        branch_guards.push(guard);
    }
    for (block, guard) in cond_blocks.iter().zip(branch_guards) {
        expanded.extend(
            block
                .stmts
                .iter()
                .map(|statement| guarded_statement(statement, guard.clone())),
        );
    }
    if let Some(fallback) = else_block {
        expanded.extend(
            fallback
                .iter()
                .map(|statement| guarded_statement(statement, remaining.clone())),
        );
    }
    Some(expanded)
}

fn statements_contain_loop(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::For { .. } | rumoca_core::Statement::While { .. } => true,
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_contain_loop(&block.stmts))
                || else_block.as_deref().is_some_and(statements_contain_loop)
        }
        _ => false,
    })
}

fn guarded_statement(
    statement: &rumoca_core::Statement,
    condition: Expression,
) -> rumoca_core::Statement {
    rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: condition,
            stmts: vec![statement.clone()],
        }],
        else_block: None,
        span: statement
            .source_span()
            .expect("conditional snapshot preserves statement provenance"),
    }
}

pub(super) fn certify_nonleading_return_branches(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    let targets = function
        .outputs
        .iter()
        .map(|output| VarName::new(&output.name))
        .collect::<Vec<_>>();
    for statement in &function.body {
        let Some((blocks, _)) = guarded_return(statement) else {
            continue;
        };
        for block in blocks {
            let (rumoca_core::Statement::Return { span }, statements) = block
                .stmts
                .split_last()
                .expect("a guarded return branch has a return")
            else {
                unreachable!("guarded_return checks the last statement")
            };
            let mut definitions = FunctionDefinitions::new(function);
            let plans = validate_function_statements(statements, context, &mut definitions)?;
            validate_return_definitions(function, statements, &plans, &targets, *span)?;
        }
    }
    Ok(())
}

pub(super) fn nonreturn_path(statements: &[rumoca_core::Statement]) -> Vec<rumoca_core::Statement> {
    statements
        .iter()
        .filter(|statement| guarded_return(statement).is_none())
        .cloned()
        .collect()
}

fn guarded_return(
    statement: &rumoca_core::Statement,
) -> Option<(&[rumoca_core::StatementBlock], Span)> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block: None,
        span,
    } = statement
    else {
        return None;
    };
    (!cond_blocks.is_empty()
        && cond_blocks.iter().all(|block| {
            matches!(
                block.stmts.last(),
                Some(rumoca_core::Statement::Return { .. })
            )
        }))
    .then_some((cond_blocks, *span))
}

fn disjoin_conditions(blocks: &[rumoca_core::StatementBlock], span: Span) -> Expression {
    blocks
        .iter()
        .map(|block| block.cond.clone())
        .reduce(|left, right| Expression::Binary {
            op: OpBinary::Or,
            lhs: Box::new(left),
            rhs: Box::new(right),
            span,
        })
        .expect("a guarded return has at least one condition")
}

fn and_condition(left: Expression, right: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(left),
        rhs: Box::new(right),
        span,
    }
}

fn validate_return_definitions(
    function: &rumoca_core::Function,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    targets: &[VarName],
    span: Span,
) -> Result<(), ToDaeError> {
    let mut assigned = HashSet::new();
    let mutable = function
        .outputs
        .iter()
        .chain(&function.locals)
        .map(|value| VarName::new(&value.name))
        .collect::<HashSet<_>>();
    for (statement, plan) in statements.iter().zip(plans) {
        let statement_span =
            required_statement_span(statement, "guarded function return definition")?;
        let (values, assignments): (Vec<&Expression>, Vec<&FunctionAssignmentPlan>) =
            match (statement, plan) {
                (
                    rumoca_core::Statement::Assignment { value, .. },
                    FunctionStatementPlan::Assignment(assignment),
                ) => (vec![value], vec![assignment]),
                (
                    rumoca_core::Statement::FunctionCall { args, .. },
                    FunctionStatementPlan::MultiOutputCall { outputs },
                ) => (args.iter().collect(), outputs.iter().flatten().collect()),
                _ => return unsupported_return_shape(function, statement),
            };
        let mut references = Vec::new();
        for value in values {
            value.collect_var_refs(&mut references);
        }
        if references
            .iter()
            .any(|reference| mutable.contains(reference))
        {
            return Err(ToDaeError::unsupported_flat(
                "function return",
                format!(
                    "`{}` requires independent whole-output definitions before return",
                    function.name
                ),
                statement_span,
            ));
        };
        for assignment in assignments {
            let target = assignment.target();
            if !assignment.is_whole()
                || !targets.contains(target)
                || !assigned.insert(target.clone())
            {
                return Err(ToDaeError::unsupported_flat(
                    "function return",
                    format!(
                        "`{}` requires independent whole-output definitions before return",
                        function.name
                    ),
                    statement_span,
                ));
            }
        }
    }
    if assigned.len() != targets.len() {
        return Err(ToDaeError::unsupported_flat(
            "function return",
            format!(
                "`{}` must define every output before returning",
                function.name
            ),
            span,
        ));
    }
    Ok(())
}

fn sequence_defines_target(plans: &[FunctionStatementPlan], target: &VarName) -> bool {
    plans.iter().any(|plan| match plan {
        FunctionStatementPlan::Assignment(assignment) if assignment.is_whole() => {
            assignment.target() == target
        }
        FunctionStatementPlan::If { targets, .. } => targets.contains(target),
        // A proven branch runs unconditionally, so it defines exactly what its
        // own statement sequence defines.
        FunctionStatementPlan::ProvenBranch { statements, .. } => {
            sequence_defines_target(statements, target)
        }
        _ => false,
    })
}

pub(super) fn contains_return(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Return { .. } => true,
        rumoca_core::Statement::For { equations, .. } => contains_return(equations),
        rumoca_core::Statement::While { block, .. } => contains_return(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| contains_return(&block.stmts))
                || else_block.as_deref().is_some_and(contains_return)
        }
        _ => false,
    })
}

fn unsupported_return_shape<T>(
    function: &rumoca_core::Function,
    statement: &rumoca_core::Statement,
) -> Result<T, ToDaeError> {
    let span = required_statement_span(statement, "unsupported guarded function return statement")?;
    Err(ToDaeError::unsupported_flat(
        "function return",
        format!(
            "`{}` requires a leading guarded return with total output definitions",
            function.name
        ),
        span,
    ))
}
