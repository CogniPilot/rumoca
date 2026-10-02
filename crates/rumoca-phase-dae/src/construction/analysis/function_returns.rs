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

/// MLS 3.7 §11.2.6: a top-level `return` ends the algorithm, so the statements
/// before it are exactly the sequence that runs (a trailing `return;` in
/// `Basic.OpAmpDetailed`'s limiters).
fn reachable_prefix(statements: &[rumoca_core::Statement]) -> &[rumoca_core::Statement] {
    statements
        .iter()
        .position(|statement| matches!(statement, rumoca_core::Statement::Return { .. }))
        .map_or(statements, |end| &statements[..end])
}

pub(super) fn normalize_function_returns(
    statements: &[rumoca_core::Statement],
) -> Result<NormalizedFunctionReturns, ToDaeError> {
    let statements = reachable_prefix(statements);
    let has_returns = contains_return(statements);
    let mut normalized = Vec::new();
    let mut guards = Vec::new();
    let mut active: Option<Expression> = None;
    for (position, statement) in statements.iter().enumerate() {
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
        if let Some(continued) =
            continue_branch_return(statement, &statements[position + 1..], active.as_ref())?
        {
            normalized.push(continued);
            break;
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
    // Each branch runs exactly when its immutable guard holds, so the prefix
    // through its last loop runs as guarded statements (the loop owner cannot
    // sit inside a value conditional), and the loop-free remainder stays one
    // conditional over the same guards. That keeps a value every remainder
    // defines defined on every path, as the source conditional did.
    let mut remainders = Vec::with_capacity(cond_blocks.len());
    for (block, guard) in cond_blocks.iter().zip(&branch_guards) {
        let remainder = hoist_loop_prefix(&block.stmts, guard, guards, &mut expanded);
        remainders.push(rumoca_core::StatementBlock {
            cond: guard.clone(),
            stmts: remainder.to_vec(),
        });
    }
    let fallback = else_block
        .as_deref()
        .map(|fallback| hoist_loop_prefix(fallback, &remaining, guards, &mut expanded).to_vec());
    let fallback = fallback.filter(|statements| !statements.is_empty());
    let complete = remainders.iter().all(|block| !block.stmts.is_empty());
    remainders.retain(|block| !block.stmts.is_empty());
    // An else part may stand for the remaining case only while every guarded
    // branch is still listed; otherwise it names its own guard.
    let fallback = match fallback {
        Some(statements) if !complete && !remainders.is_empty() => {
            remainders.push(rumoca_core::StatementBlock {
                cond: remaining.clone(),
                stmts: statements,
            });
            None
        }
        fallback => fallback,
    };
    match (remainders.is_empty(), fallback) {
        (true, None) => {}
        (true, Some(fallback)) => expanded.extend(
            fallback
                .iter()
                .map(|statement| guarded_statement(statement, remaining.clone())),
        ),
        (false, fallback) => expanded.push(rumoca_core::Statement::If {
            cond_blocks: remainders,
            else_block: fallback,
            span: *span,
        }),
    }
    Some(expanded)
}

/// Emit `statements` through their last loop as statements guarded by
/// `guard`, and return the loop-free remainder.
fn hoist_loop_prefix<'a>(
    statements: &'a [rumoca_core::Statement],
    guard: &Expression,
    guards: &mut Vec<GeneratedBooleanDefinition>,
    expanded: &mut Vec<rumoca_core::Statement>,
) -> &'a [rumoca_core::Statement] {
    let split = statements
        .iter()
        .rposition(|statement| statements_contain_loop(std::slice::from_ref(statement)))
        .map_or(0, |last| last + 1);
    for statement in &statements[..split] {
        match snapshot_loop_conditional(statement, Some(guard), guards) {
            Some(nested) => expanded.extend(nested),
            None => expanded.push(guarded_statement(statement, guard.clone())),
        }
    }
    &statements[split..]
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

/// An `if` statement some of whose branches end in `return` (MLS 3.7
/// §11.2.6), with no other `return` inside it: the else part of IF97-style
/// dispatch (`Modelica.Fluid.Utilities.regRoot2_utility` returns from its
/// else branch).
struct BranchReturn<'statement> {
    cond_blocks: &'statement [rumoca_core::StatementBlock],
    else_block: Option<&'statement [rumoca_core::Statement]>,
    span: Span,
}

/// MLS 3.7 §11.2.6: the statements after a partially returning conditional
/// run exactly on its non-returning branches, so they continue each of those
/// branches; the result runs under the guard of earlier returns. Any other
/// nested `return` is refused.
fn continue_branch_return(
    statement: &rumoca_core::Statement,
    rest: &[rumoca_core::Statement],
    active: Option<&Expression>,
) -> Result<Option<rumoca_core::Statement>, ToDaeError> {
    let Some(branches) = branch_return(statement) else {
        if contains_return(std::slice::from_ref(statement)) {
            let span = required_statement_span(statement, "nested function return")?;
            return Err(ToDaeError::unsupported_flat(
                "function return",
                "a non-guarded nested return requires a checked control-flow owner",
                span,
            ));
        }
        return Ok(None);
    };
    if contains_return(rest) {
        let span = required_statement_span(statement, "nested function return")?;
        return Err(ToDaeError::unsupported_flat(
            "function return",
            "a return after a partially returning conditional requires a checked control-flow owner",
            span,
        ));
    }
    let continued = branches.continued_by(rest);
    Ok(Some(match active {
        Some(active) => guarded_statement(&continued, active.clone()),
        None => continued,
    }))
}

fn ends_in_return(statements: &[rumoca_core::Statement]) -> bool {
    matches!(
        statements.last(),
        Some(rumoca_core::Statement::Return { .. })
    ) && !contains_return(&statements[..statements.len() - 1])
}

fn branch_return(statement: &rumoca_core::Statement) -> Option<BranchReturn<'_>> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    else {
        return None;
    };
    let branches = cond_blocks
        .iter()
        .map(|block| block.stmts.as_slice())
        .chain(else_block.as_deref());
    let mut returning = false;
    for statements in branches {
        if ends_in_return(statements) {
            returning = true;
        } else if contains_return(statements) {
            return None;
        }
    }
    returning.then_some(BranchReturn {
        cond_blocks,
        else_block: else_block.as_deref(),
        span: *span,
    })
}

impl BranchReturn<'_> {
    /// The conditional whose returning branches end at their `return` and
    /// whose other branches, including an absent else part, continue with
    /// `rest`.
    fn continued_by(&self, rest: &[rumoca_core::Statement]) -> rumoca_core::Statement {
        let continue_with = |statements: &[rumoca_core::Statement]| {
            if ends_in_return(statements) {
                statements[..statements.len() - 1].to_vec()
            } else {
                statements.iter().chain(rest).cloned().collect()
            }
        };
        let cond_blocks = self
            .cond_blocks
            .iter()
            .map(|block| rumoca_core::StatementBlock {
                cond: block.cond.clone(),
                stmts: continue_with(&block.stmts),
            })
            .collect();
        let else_block = continue_with(self.else_block.unwrap_or_default());
        rumoca_core::Statement::If {
            cond_blocks,
            else_block: (!else_block.is_empty()).then_some(else_block),
            span: self.span,
        }
    }
}
