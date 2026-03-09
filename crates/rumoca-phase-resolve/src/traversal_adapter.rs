use rumoca_core::ScopeId;
use rumoca_ir_ast as ast;
use std::sync::Arc;

type ComponentReference = ast::ComponentReference;
type Equation = ast::Equation;
type Expression = ast::Expression;
type Statement = ast::Statement;

/// Callback contract for resolve traversal.
///
/// Traversal is centralized here while semantic actions stay in callbacks.
pub(crate) trait ResolveTraversalCallbacks {
    fn create_loop_scope(&mut self, parent_scope: ScopeId) -> ScopeId;
    fn bind_loop_index_name(&mut self, loop_scope: ScopeId, index_name: &str);
    fn on_component_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId);
    fn on_function_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId);
}

pub(crate) fn walk_equations<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    equations: &mut [Equation],
    scope: ScopeId,
) {
    for equation in equations {
        walk_equation(callbacks, equation, scope);
    }
}

pub(crate) fn walk_equation<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    equation: &mut Equation,
    scope: ScopeId,
) {
    match equation {
        Equation::Simple { lhs, rhs } => {
            walk_expression(callbacks, lhs, scope);
            walk_expression(callbacks, rhs, scope);
        }
        Equation::Connect { lhs, rhs } => {
            callbacks.on_component_reference(lhs, scope);
            callbacks.on_component_reference(rhs, scope);
        }
        Equation::For { indices, equations } => {
            let loop_scope = callbacks.create_loop_scope(scope);
            for index in indices {
                callbacks.bind_loop_index_name(loop_scope, index.ident.text.as_ref());
                walk_expression(callbacks, &mut index.range, loop_scope);
            }
            walk_equations(callbacks, equations, loop_scope);
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                walk_expression(callbacks, &mut block.cond, scope);
                walk_equations(callbacks, &mut block.eqs, scope);
            }
            if let Some(else_equations) = else_block {
                walk_equations(callbacks, else_equations, scope);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                walk_expression(callbacks, &mut block.cond, scope);
                walk_equations(callbacks, &mut block.eqs, scope);
            }
        }
        Equation::FunctionCall { comp, args } => {
            callbacks.on_function_reference(comp, scope);
            walk_expressions(callbacks, args, scope);
        }
        Equation::Assert {
            condition,
            message,
            level,
        } => {
            walk_expression(callbacks, condition, scope);
            walk_expression(callbacks, message, scope);
            if let Some(level_expr) = level {
                walk_expression(callbacks, level_expr, scope);
            }
        }
        Equation::Empty => {}
    }
}

pub(crate) fn walk_statements<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    statements: &mut [Statement],
    scope: ScopeId,
) {
    for statement in statements {
        walk_statement(callbacks, statement, scope);
    }
}

pub(crate) fn walk_statement<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    statement: &mut Statement,
    scope: ScopeId,
) {
    match statement {
        Statement::Assignment { comp, value } => {
            callbacks.on_component_reference(comp, scope);
            walk_expression(callbacks, value, scope);
        }
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            callbacks.on_function_reference(comp, scope);
            walk_expressions(callbacks, args, scope);
            walk_expressions(callbacks, outputs, scope);
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                walk_expression(callbacks, &mut block.cond, scope);
                walk_statements(callbacks, &mut block.stmts, scope);
            }
            if let Some(else_statements) = else_block {
                walk_statements(callbacks, else_statements, scope);
            }
        }
        Statement::For { indices, equations } => {
            let loop_scope = callbacks.create_loop_scope(scope);
            for index in indices {
                callbacks.bind_loop_index_name(loop_scope, index.ident.text.as_ref());
                walk_expression(callbacks, &mut index.range, loop_scope);
            }
            walk_statements(callbacks, equations, loop_scope);
        }
        Statement::While(block) => {
            walk_expression(callbacks, &mut block.cond, scope);
            walk_statements(callbacks, &mut block.stmts, scope);
        }
        Statement::When(blocks) => {
            for block in blocks {
                walk_expression(callbacks, &mut block.cond, scope);
                walk_statements(callbacks, &mut block.stmts, scope);
            }
        }
        Statement::Reinit { variable, value } => {
            callbacks.on_component_reference(variable, scope);
            walk_expression(callbacks, value, scope);
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            walk_expression(callbacks, condition, scope);
            walk_expression(callbacks, message, scope);
            if let Some(level_expr) = level {
                walk_expression(callbacks, level_expr, scope);
            }
        }
        Statement::Return { .. } | Statement::Break { .. } | Statement::Empty => {}
    }
}

pub(crate) fn walk_expressions<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    expressions: &mut [Expression],
    scope: ScopeId,
) {
    for expression in expressions {
        walk_expression(callbacks, expression, scope);
    }
}

pub(crate) fn walk_expression<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    expression: &mut Expression,
    scope: ScopeId,
) {
    match expression {
        Expression::ComponentReference(comp) => callbacks.on_component_reference(comp, scope),
        Expression::FunctionCall { comp, args } => {
            callbacks.on_function_reference(comp, scope);
            walk_expressions(callbacks, args, scope);
        }
        Expression::Binary { lhs, rhs, .. } => {
            walk_expression(callbacks, Arc::make_mut(lhs), scope);
            walk_expression(callbacks, Arc::make_mut(rhs), scope);
        }
        Expression::Unary { rhs, .. } => walk_expression(callbacks, Arc::make_mut(rhs), scope),
        Expression::Range { start, step, end } => {
            walk_expression(callbacks, Arc::make_mut(start), scope);
            if let Some(step_expression) = step {
                walk_expression(callbacks, Arc::make_mut(step_expression), scope);
            }
            walk_expression(callbacks, Arc::make_mut(end), scope);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            walk_expressions(callbacks, elements, scope);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, then_expr) in branches {
                walk_expression(callbacks, condition, scope);
                walk_expression(callbacks, then_expr, scope);
            }
            walk_expression(callbacks, Arc::make_mut(else_branch), scope);
        }
        Expression::ClassModification {
            target,
            modifications,
        } => {
            callbacks.on_function_reference(target, scope);
            walk_expressions(callbacks, modifications, scope);
        }
        Expression::Modification { target, value } => {
            callbacks.on_component_reference(target, scope);
            walk_expression(callbacks, Arc::make_mut(value), scope);
        }
        Expression::NamedArgument { value, .. } => {
            walk_expression(callbacks, Arc::make_mut(value), scope);
        }
        Expression::Parenthesized { inner } => {
            walk_expression(callbacks, Arc::make_mut(inner), scope);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            let loop_scope = callbacks.create_loop_scope(scope);
            for index in indices {
                callbacks.bind_loop_index_name(loop_scope, index.ident.text.as_ref());
                walk_expression(callbacks, &mut index.range, loop_scope);
            }
            walk_expression(callbacks, Arc::make_mut(expr), loop_scope);
            if let Some(filter_expr) = filter {
                walk_expression(callbacks, Arc::make_mut(filter_expr), loop_scope);
            }
        }
        Expression::ArrayIndex { base, subscripts } => {
            walk_expression(callbacks, Arc::make_mut(base), scope);
            walk_subscripts(callbacks, subscripts, scope);
        }
        Expression::FieldAccess { base, .. } => {
            walk_expression(callbacks, Arc::make_mut(base), scope);
        }
        Expression::Terminal { .. } | Expression::Empty => {}
    }
}

pub(crate) fn walk_subscripts<C: ResolveTraversalCallbacks>(
    callbacks: &mut C,
    subscripts: &mut [ast::Subscript],
    scope: ScopeId,
) {
    for subscript in subscripts {
        if let ast::Subscript::Expression(expression) = subscript {
            walk_expression(callbacks, expression, scope);
        }
    }
}
