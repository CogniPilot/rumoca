use super::*;

pub(super) fn substitute_subscript_with_bindings(
    subscript: &Subscript,
    bindings: &HashMap<String, i64>,
) -> Subscript {
    match subscript {
        Subscript::Index(index) => Subscript::Index(*index),
        Subscript::Colon => Subscript::Colon,
        Subscript::Expr(expr) => {
            Subscript::Expr(Box::new(substitute_expr_with_bindings(expr, bindings)))
        }
    }
}

pub(super) fn substitute_expr_with_bindings(
    expr: &Expression,
    bindings: &HashMap<String, i64>,
) -> Expression {
    match expr {
        Expression::VarRef { name, subscripts } => {
            substitute_var_ref_with_bindings(name, subscripts, bindings)
        }
        Expression::Binary { op, lhs, rhs } => {
            substitute_binary_expr_with_bindings(op, lhs, rhs, bindings)
        }
        Expression::Unary { op, rhs } => substitute_unary_expr_with_bindings(op, rhs, bindings),
        Expression::BuiltinCall { function, args } => {
            substitute_builtin_call_with_bindings(*function, args, bindings)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => substitute_function_call_with_bindings(name, args, *is_constructor, bindings),
        Expression::Literal(literal) => Expression::Literal(literal.clone()),
        Expression::If {
            branches,
            else_branch,
        } => substitute_if_expr_with_bindings(branches, else_branch, bindings),
        Expression::Array {
            elements,
            is_matrix,
        } => substitute_array_expr_with_bindings(elements, *is_matrix, bindings),
        Expression::Tuple { elements } => substitute_tuple_expr_with_bindings(elements, bindings),
        Expression::Range { start, step, end } => {
            substitute_range_expr_with_bindings(start, step, end, bindings)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => substitute_array_comprehension_with_bindings(expr, indices, filter, bindings),
        Expression::Index { base, subscripts } => {
            substitute_index_expr_with_bindings(base, subscripts, bindings)
        }
        Expression::FieldAccess { base, field } => {
            substitute_field_access_with_bindings(base, field, bindings)
        }
        Expression::Empty => Expression::Empty,
    }
}

pub(super) fn substitute_var_ref_with_bindings(
    name: &VarName,
    subscripts: &[Subscript],
    bindings: &HashMap<String, i64>,
) -> Expression {
    if subscripts.is_empty()
        && let Some(value) = bindings.get(name.as_str())
    {
        return Expression::Literal(Literal::Integer(*value));
    }
    Expression::VarRef {
        name: name.clone(),
        subscripts: subscripts
            .iter()
            .map(|subscript| substitute_subscript_with_bindings(subscript, bindings))
            .collect(),
    }
}

pub(super) fn substitute_binary_expr_with_bindings(
    op: &ast::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::Binary {
        op: op.clone(),
        lhs: Box::new(substitute_expr_with_bindings(lhs, bindings)),
        rhs: Box::new(substitute_expr_with_bindings(rhs, bindings)),
    }
}

pub(super) fn substitute_unary_expr_with_bindings(
    op: &ast::OpUnary,
    rhs: &Expression,
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::Unary {
        op: op.clone(),
        rhs: Box::new(substitute_expr_with_bindings(rhs, bindings)),
    }
}

pub(super) fn substitute_builtin_call_with_bindings(
    function: BuiltinFunction,
    args: &[Expression],
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::BuiltinCall {
        function,
        args: args
            .iter()
            .map(|arg| substitute_expr_with_bindings(arg, bindings))
            .collect(),
    }
}

pub(super) fn substitute_function_call_with_bindings(
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::FunctionCall {
        name: name.clone(),
        args: args
            .iter()
            .map(|arg| substitute_expr_with_bindings(arg, bindings))
            .collect(),
        is_constructor,
    }
}

pub(super) fn substitute_if_expr_with_bindings(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, value)| {
                (
                    substitute_expr_with_bindings(cond, bindings),
                    substitute_expr_with_bindings(value, bindings),
                )
            })
            .collect(),
        else_branch: Box::new(substitute_expr_with_bindings(else_branch, bindings)),
    }
}

pub(super) fn substitute_array_expr_with_bindings(
    elements: &[Expression],
    is_matrix: bool,
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::Array {
        elements: elements
            .iter()
            .map(|element| substitute_expr_with_bindings(element, bindings))
            .collect(),
        is_matrix,
    }
}

pub(super) fn substitute_tuple_expr_with_bindings(
    elements: &[Expression],
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::Tuple {
        elements: elements
            .iter()
            .map(|element| substitute_expr_with_bindings(element, bindings))
            .collect(),
    }
}

pub(super) fn substitute_range_expr_with_bindings(
    start: &Expression,
    step: &Option<Box<Expression>>,
    end: &Expression,
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::Range {
        start: Box::new(substitute_expr_with_bindings(start, bindings)),
        step: step
            .as_ref()
            .map(|step_expr| Box::new(substitute_expr_with_bindings(step_expr, bindings))),
        end: Box::new(substitute_expr_with_bindings(end, bindings)),
    }
}

pub(super) fn substitute_array_comprehension_with_bindings(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: &Option<Box<Expression>>,
    bindings: &HashMap<String, i64>,
) -> Expression {
    let local_bindings = bindings_without_comprehension_indices(bindings, indices);
    Expression::ArrayComprehension {
        expr: Box::new(substitute_expr_with_bindings(expr, &local_bindings)),
        indices: indices
            .iter()
            .map(|index| ComprehensionIndex {
                name: index.name.clone(),
                range: substitute_expr_with_bindings(&index.range, bindings),
            })
            .collect(),
        filter: filter.as_ref().map(|filter_expr| {
            Box::new(substitute_expr_with_bindings(filter_expr, &local_bindings))
        }),
    }
}

pub(super) fn bindings_without_comprehension_indices(
    bindings: &HashMap<String, i64>,
    indices: &[ComprehensionIndex],
) -> HashMap<String, i64> {
    let mut local_bindings = bindings.clone();
    for index in indices {
        local_bindings.remove(&index.name);
    }
    local_bindings
}

pub(super) fn substitute_index_expr_with_bindings(
    base: &Expression,
    subscripts: &[Subscript],
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::Index {
        base: Box::new(substitute_expr_with_bindings(base, bindings)),
        subscripts: subscripts
            .iter()
            .map(|subscript| substitute_subscript_with_bindings(subscript, bindings))
            .collect(),
    }
}

pub(super) fn substitute_field_access_with_bindings(
    base: &Expression,
    field: &str,
    bindings: &HashMap<String, i64>,
) -> Expression {
    Expression::FieldAccess {
        base: Box::new(substitute_expr_with_bindings(base, bindings)),
        field: field.to_string(),
    }
}

pub(super) fn substitute_component_ref_with_bindings(
    comp: &ComponentReference,
    bindings: &HashMap<String, i64>,
) -> ComponentReference {
    ComponentReference {
        local: comp.local,
        parts: comp
            .parts
            .iter()
            .map(|part| flat::ComponentRefPart {
                ident: part.ident.clone(),
                subs: part
                    .subs
                    .iter()
                    .map(|subscript| substitute_subscript_with_bindings(subscript, bindings))
                    .collect(),
            })
            .collect(),
        def_id: comp.def_id,
    }
}

pub(super) fn substitute_statement_with_bindings(
    statement: &Statement,
    bindings: &HashMap<String, i64>,
) -> Statement {
    match statement {
        Statement::Empty => Statement::Empty,
        Statement::Assignment { comp, value } => {
            substitute_assignment_statement_with_bindings(comp, value, bindings)
        }
        Statement::Return => Statement::Return,
        Statement::Break => Statement::Break,
        Statement::For { indices, equations } => {
            substitute_for_statement_with_bindings(indices, equations, bindings)
        }
        Statement::While(block) => {
            Statement::While(substitute_statement_block_with_bindings(block, bindings))
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => substitute_if_statement_with_bindings(cond_blocks, else_block, bindings),
        Statement::When(blocks) => substitute_when_statement_with_bindings(blocks, bindings),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => substitute_function_call_statement_with_bindings(comp, args, outputs, bindings),
        Statement::Reinit { variable, value } => {
            substitute_reinit_statement_with_bindings(variable, value, bindings)
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => substitute_assert_statement_with_bindings(condition, message, level, bindings),
    }
}

pub(super) fn substitute_assignment_statement_with_bindings(
    comp: &ComponentReference,
    value: &Expression,
    bindings: &HashMap<String, i64>,
) -> Statement {
    Statement::Assignment {
        comp: substitute_component_ref_with_bindings(comp, bindings),
        value: substitute_expr_with_bindings(value, bindings),
    }
}

pub(super) fn substitute_statement_block_with_bindings(
    block: &StatementBlock,
    bindings: &HashMap<String, i64>,
) -> StatementBlock {
    StatementBlock {
        cond: substitute_expr_with_bindings(&block.cond, bindings),
        stmts: block
            .stmts
            .iter()
            .map(|nested| substitute_statement_with_bindings(nested, bindings))
            .collect(),
    }
}

pub(super) fn substitute_for_statement_with_bindings(
    indices: &[flat::ForIndex],
    equations: &[Statement],
    bindings: &HashMap<String, i64>,
) -> Statement {
    let nested_bindings = bindings_without_for_indices(bindings, indices);
    Statement::For {
        indices: indices
            .iter()
            .map(|index| flat::ForIndex {
                ident: index.ident.clone(),
                range: substitute_expr_with_bindings(&index.range, bindings),
            })
            .collect(),
        equations: equations
            .iter()
            .map(|nested| substitute_statement_with_bindings(nested, &nested_bindings))
            .collect(),
    }
}

pub(super) fn bindings_without_for_indices(
    bindings: &HashMap<String, i64>,
    indices: &[flat::ForIndex],
) -> HashMap<String, i64> {
    let mut nested_bindings = bindings.clone();
    for index in indices {
        nested_bindings.remove(&index.ident);
    }
    nested_bindings
}

pub(super) fn substitute_if_statement_with_bindings(
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
    bindings: &HashMap<String, i64>,
) -> Statement {
    Statement::If {
        cond_blocks: cond_blocks
            .iter()
            .map(|block| substitute_statement_block_with_bindings(block, bindings))
            .collect(),
        else_block: else_block.as_ref().map(|stmts| {
            stmts
                .iter()
                .map(|nested| substitute_statement_with_bindings(nested, bindings))
                .collect()
        }),
    }
}

pub(super) fn substitute_when_statement_with_bindings(
    blocks: &[StatementBlock],
    bindings: &HashMap<String, i64>,
) -> Statement {
    Statement::When(
        blocks
            .iter()
            .map(|block| substitute_statement_block_with_bindings(block, bindings))
            .collect(),
    )
}

pub(super) fn substitute_function_call_statement_with_bindings(
    comp: &ComponentReference,
    args: &[Expression],
    outputs: &[Expression],
    bindings: &HashMap<String, i64>,
) -> Statement {
    Statement::FunctionCall {
        comp: substitute_component_ref_with_bindings(comp, bindings),
        args: args
            .iter()
            .map(|arg| substitute_expr_with_bindings(arg, bindings))
            .collect(),
        outputs: outputs
            .iter()
            .map(|output| substitute_expr_with_bindings(output, bindings))
            .collect(),
    }
}

pub(super) fn substitute_reinit_statement_with_bindings(
    variable: &ComponentReference,
    value: &Expression,
    bindings: &HashMap<String, i64>,
) -> Statement {
    Statement::Reinit {
        variable: substitute_component_ref_with_bindings(variable, bindings),
        value: substitute_expr_with_bindings(value, bindings),
    }
}

pub(super) fn substitute_assert_statement_with_bindings(
    condition: &Expression,
    message: &Expression,
    level: &Option<Expression>,
    bindings: &HashMap<String, i64>,
) -> Statement {
    Statement::Assert {
        condition: substitute_expr_with_bindings(condition, bindings),
        message: substitute_expr_with_bindings(message, bindings),
        level: level
            .as_ref()
            .map(|level_expr| substitute_expr_with_bindings(level_expr, bindings)),
    }
}
