use rumoca_ir_ast as ast;

type Equation = ast::Equation;
type Expression = ast::Expression;
type Statement = ast::Statement;
type TypeTable = ast::TypeTable;

/// Callback contract for typecheck traversal.
///
/// Traversal is centralized here while typecheck-specific semantic actions
/// are injected via callbacks.
pub(crate) trait TypeCheckTraversalCallbacks {
    /// Called after both sides of a simple equation are traversed.
    fn on_simple_equation(&mut self, lhs: &Expression, rhs: &Expression, type_table: &TypeTable);
}

pub(crate) fn walk_equations<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    equations: &[Equation],
    type_table: &TypeTable,
) {
    for equation in equations {
        walk_equation(callbacks, equation, type_table);
    }
}

pub(crate) fn walk_equation<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    equation: &Equation,
    type_table: &TypeTable,
) {
    match equation {
        Equation::Empty | Equation::Connect { .. } => {}
        Equation::Simple { lhs, rhs } => {
            walk_expression(callbacks, lhs, type_table);
            walk_expression(callbacks, rhs, type_table);
            callbacks.on_simple_equation(lhs, rhs, type_table);
        }
        Equation::For {
            indices: _,
            equations,
        } => {
            walk_equations(callbacks, equations, type_table);
        }
        Equation::When(blocks) => {
            for block in blocks {
                walk_expression(callbacks, &block.cond, type_table);
                walk_equations(callbacks, &block.eqs, type_table);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                walk_expression(callbacks, &block.cond, type_table);
                walk_equations(callbacks, &block.eqs, type_table);
            }
            if let Some(else_equations) = else_block {
                walk_equations(callbacks, else_equations, type_table);
            }
        }
        Equation::FunctionCall { comp: _, args } => walk_expressions(callbacks, args, type_table),
        Equation::Assert {
            condition,
            message,
            level,
        } => {
            walk_expression(callbacks, condition, type_table);
            walk_expression(callbacks, message, type_table);
            if let Some(level_expression) = level {
                walk_expression(callbacks, level_expression, type_table);
            }
        }
    }
}

pub(crate) fn walk_statements<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    statements: &[Statement],
    type_table: &TypeTable,
) {
    for statement in statements {
        walk_statement(callbacks, statement, type_table);
    }
}

pub(crate) fn walk_statement<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    statement: &Statement,
    type_table: &TypeTable,
) {
    match statement {
        Statement::Empty | Statement::Return { .. } | Statement::Break { .. } => {}
        Statement::Assignment { comp: _, value } => {
            walk_expression(callbacks, value, type_table);
        }
        Statement::FunctionCall {
            comp: _,
            args,
            outputs,
        } => {
            walk_expressions(callbacks, args, type_table);
            walk_expressions(callbacks, outputs, type_table);
        }
        Statement::For {
            indices: _,
            equations,
        } => {
            walk_statements(callbacks, equations, type_table);
        }
        Statement::While(block) => {
            walk_expression(callbacks, &block.cond, type_table);
            walk_statements(callbacks, &block.stmts, type_table);
        }
        Statement::When(blocks) => {
            for block in blocks {
                walk_expression(callbacks, &block.cond, type_table);
                walk_statements(callbacks, &block.stmts, type_table);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                walk_expression(callbacks, &block.cond, type_table);
                walk_statements(callbacks, &block.stmts, type_table);
            }
            if let Some(else_statements) = else_block {
                walk_statements(callbacks, else_statements, type_table);
            }
        }
        Statement::Reinit { variable: _, value } => {
            walk_expression(callbacks, value, type_table);
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            walk_expression(callbacks, condition, type_table);
            walk_expression(callbacks, message, type_table);
            if let Some(level_expression) = level {
                walk_expression(callbacks, level_expression, type_table);
            }
        }
    }
}

pub(crate) fn walk_expressions<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    expressions: &[Expression],
    type_table: &TypeTable,
) {
    for expression in expressions {
        walk_expression(callbacks, expression, type_table);
    }
}

pub(crate) fn walk_expression<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    expression: &Expression,
    type_table: &TypeTable,
) {
    match expression {
        Expression::Empty | Expression::Terminal { .. } | Expression::ComponentReference(_) => {}
        Expression::Range { start, step, end } => {
            walk_expression(callbacks, start, type_table);
            if let Some(step_expression) = step {
                walk_expression(callbacks, step_expression, type_table);
            }
            walk_expression(callbacks, end, type_table);
        }
        Expression::Unary { op: _, rhs } => walk_expression(callbacks, rhs, type_table),
        Expression::Binary { op: _, lhs, rhs } => {
            walk_expression(callbacks, lhs, type_table);
            walk_expression(callbacks, rhs, type_table);
        }
        Expression::FunctionCall { comp: _, args } => walk_expressions(callbacks, args, type_table),
        Expression::ClassModification {
            target: _,
            modifications,
        } => walk_expressions(callbacks, modifications, type_table),
        Expression::NamedArgument { name: _, value } => {
            walk_expression(callbacks, value, type_table);
        }
        Expression::Modification { target: _, value } => {
            walk_expression(callbacks, value, type_table);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            walk_expressions(callbacks, elements, type_table);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, then_expression) in branches {
                walk_expression(callbacks, condition, type_table);
                walk_expression(callbacks, then_expression, type_table);
            }
            walk_expression(callbacks, else_branch, type_table);
        }
        Expression::Parenthesized { inner } => walk_expression(callbacks, inner, type_table),
        Expression::ArrayComprehension {
            expr,
            indices: _,
            filter,
        } => {
            walk_expression(callbacks, expr, type_table);
            if let Some(filter_expression) = filter {
                walk_expression(callbacks, filter_expression, type_table);
            }
        }
        Expression::ArrayIndex { base, subscripts } => {
            walk_expression(callbacks, base, type_table);
            walk_subscripts(callbacks, subscripts, type_table);
        }
        Expression::FieldAccess { base, .. } => {
            walk_expression(callbacks, base, type_table);
        }
    }
}

pub(crate) fn walk_subscripts<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    subscripts: &[ast::Subscript],
    type_table: &TypeTable,
) {
    for subscript in subscripts {
        if let ast::Subscript::Expression(expression) = subscript {
            walk_expression(callbacks, expression, type_table);
        }
    }
}
