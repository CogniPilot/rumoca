//! Structural closure for raw GALEC syntax before semantic analyses.
//!
//! Keeping these checks at the checked-block constructor boundary makes
//! malformed shapes unable to reach MiniJinja, the evaluator, or package
//! construction.

use crate::ast::{
    Condition, Dimension, Expression, FunctionCall, Reference, Statement, VariableDeclaration,
};
use crate::diagnostic::{GalecError, PathSegment};

use super::context::{BlockContext, Cursor, lexeme, reference_parts};

pub(super) fn check(ctx: &BlockContext<'_>, diagnostics: &mut Vec<GalecError>) {
    let mut cursor = Cursor::for_block(ctx);
    for variable in &ctx.block.interface {
        cursor.push(PathSegment::Variable(lexeme(&variable.decl.name)));
        declaration(&variable.decl, &cursor, diagnostics);
        optional_expression(variable.start.as_ref(), &cursor, diagnostics);
        cursor.pop();
    }
    for variable in &ctx.block.protected {
        cursor.push(PathSegment::Variable(lexeme(&variable.decl.name)));
        declaration(&variable.decl, &cursor, diagnostics);
        optional_expression(variable.start.as_ref(), &cursor, diagnostics);
        cursor.pop();
    }
    for compartment in &ctx.block.compartments {
        cursor.push(PathSegment::Compartment(lexeme(&compartment.name)));
        for variable in &compartment.entities {
            cursor.push(PathSegment::Variable(lexeme(&variable.decl.name)));
            declaration(&variable.decl, &cursor, diagnostics);
            optional_expression(variable.start.as_ref(), &cursor, diagnostics);
            cursor.pop();
        }
        cursor.pop();
    }
    for body in ctx.bodies() {
        let mut cursor = Cursor::for_body(ctx, &body);
        for parameter in body.parameters {
            cursor.push(PathSegment::Parameter(lexeme(&parameter.decl.name)));
            declaration(&parameter.decl, &cursor, diagnostics);
            cursor.pop();
        }
        for local in body.locals {
            cursor.push(PathSegment::Local(lexeme(&local.name)));
            declaration(local, &cursor, diagnostics);
            cursor.pop();
        }
        statements(body.statements, &mut cursor, diagnostics);
    }
}

fn declaration(
    declaration: &VariableDeclaration,
    cursor: &Cursor,
    diagnostics: &mut Vec<GalecError>,
) {
    for dimension in &declaration.dimensions {
        if let Dimension::Expr(expression) = dimension {
            check_expression(expression, cursor, diagnostics);
        }
    }
    optional_expression(declaration.range.min.as_ref(), cursor, diagnostics);
    optional_expression(declaration.range.max.as_ref(), cursor, diagnostics);
}

fn optional_expression(
    expression: Option<&Expression>,
    cursor: &Cursor,
    diagnostics: &mut Vec<GalecError>,
) {
    if let Some(expression) = expression {
        check_expression(expression, cursor, diagnostics);
    }
}

fn statements(
    values: &[crate::ast::Spanned<Statement>],
    cursor: &mut Cursor,
    diagnostics: &mut Vec<GalecError>,
) {
    for (index, statement) in values.iter().enumerate() {
        cursor.push(PathSegment::Statement(index));
        check_statement(&statement.node, cursor, diagnostics);
        cursor.pop();
    }
}

fn check_statement(statement: &Statement, cursor: &mut Cursor, diagnostics: &mut Vec<GalecError>) {
    match statement {
        Statement::Assignment { target, value } => {
            reference(target, cursor, diagnostics);
            check_expression(value, cursor, diagnostics);
        }
        Statement::MultiAssignment { targets, call } => {
            for target in targets {
                reference(target, cursor, diagnostics);
            }
            function_call(call, cursor, diagnostics);
        }
        Statement::Call(call) => function_call(call, cursor, diagnostics),
        Statement::If(value) => {
            if value.branches.is_empty() {
                diagnostics.push(GalecError::IfStatementWithoutBranches {
                    location: cursor.here(),
                });
            }
            for (index, branch) in value.branches.iter().enumerate() {
                cursor.push(PathSegment::Branch(index));
                cursor.push(PathSegment::Condition);
                condition(&branch.condition, cursor, diagnostics);
                cursor.pop();
                statements(&branch.body, cursor, diagnostics);
                cursor.pop();
            }
            if let Some(body) = &value.else_body {
                cursor.push(PathSegment::Else);
                statements(body, cursor, diagnostics);
                cursor.pop();
            }
        }
        Statement::For(value) => {
            check_expression(&value.start, cursor, diagnostics);
            optional_expression(value.step.as_ref(), cursor, diagnostics);
            check_expression(&value.stop, cursor, diagnostics);
            statements(&value.body, cursor, diagnostics);
        }
        Statement::Limit(targets) => {
            if targets.is_empty() {
                diagnostics.push(GalecError::EmptyLimitStatement {
                    location: cursor.here(),
                });
            }
            for target in targets {
                if let crate::ast::LimitTarget::Reference(target) = target {
                    reference(target, cursor, diagnostics);
                }
            }
        }
        Statement::Signal(signals) if signals.is_empty() => {
            diagnostics.push(GalecError::EmptySignalStatement {
                location: cursor.here(),
            });
        }
        Statement::Signal(_) => {}
    }
}

fn condition(condition: &Condition, cursor: &Cursor, diagnostics: &mut Vec<GalecError>) {
    match condition {
        Condition::Expression(expression) => check_expression(expression, cursor, diagnostics),
        Condition::SignalCheck(check) => {
            if check
                .test
                .as_ref()
                .is_some_and(|test| test.signals.is_empty())
            {
                diagnostics.push(GalecError::EmptySignalTestList {
                    location: cursor.here(),
                });
            }
            optional_expression(check.fallback.as_ref(), cursor, diagnostics);
        }
    }
}

fn check_expression(expression: &Expression, cursor: &Cursor, diagnostics: &mut Vec<GalecError>) {
    match expression {
        Expression::Real(value) if !value.is_finite() => {
            diagnostics.push(GalecError::NonFiniteRealLiteral {
                location: cursor.here(),
                value: *value,
            });
        }
        Expression::Ref(value) | Expression::Neg(value) => reference(value, cursor, diagnostics),
        Expression::Size { array, dimension } => {
            reference(array, cursor, diagnostics);
            check_expression(dimension, cursor, diagnostics);
        }
        Expression::Call(call) => function_call(call, cursor, diagnostics),
        Expression::Paren(value) | Expression::Not(value) => {
            check_expression(value, cursor, diagnostics);
        }
        Expression::If(value) => {
            if value.branches.is_empty() {
                diagnostics.push(GalecError::IfExpressionWithoutBranches {
                    location: cursor.here(),
                });
            }
            for (condition, result) in &value.branches {
                check_expression(condition, cursor, diagnostics);
                check_expression(result, cursor, diagnostics);
            }
            check_expression(&value.else_value, cursor, diagnostics);
        }
        Expression::Array(values) => {
            if values.is_empty() {
                diagnostics.push(GalecError::EmptyArrayConstructor {
                    location: cursor.here(),
                });
            }
            for value in values {
                check_expression(value, cursor, diagnostics);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expression(lhs, cursor, diagnostics);
            check_expression(rhs, cursor, diagnostics);
        }
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
    }
}

fn function_call(call: &FunctionCall, cursor: &Cursor, diagnostics: &mut Vec<GalecError>) {
    for argument in &call.arguments {
        check_expression(argument, cursor, diagnostics);
    }
}

fn reference(value: &Reference, cursor: &Cursor, diagnostics: &mut Vec<GalecError>) {
    for part in reference_parts(value) {
        for subscript in &part.subscripts {
            check_expression(subscript, cursor, diagnostics);
        }
    }
}
