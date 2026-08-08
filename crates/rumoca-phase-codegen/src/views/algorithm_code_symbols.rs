//! Closed target-neutral symbol inventory for checked Algorithm Code.

use rumoca_ir_galec::ast;

pub(super) fn collect(block: &ast::Block) -> Vec<&str> {
    let mut names = vec![block.name.lexeme()];
    for variable in &block.interface {
        collect_declaration(&variable.decl, &mut names);
        collect_optional_expression(variable.start.as_ref(), &mut names);
    }
    for compartment in &block.compartments {
        names.push(compartment.name.lexeme());
        for variable in &compartment.entities {
            collect_declaration(&variable.decl, &mut names);
            collect_optional_expression(variable.start.as_ref(), &mut names);
        }
    }
    for variable in &block.protected {
        collect_declaration(&variable.decl, &mut names);
        collect_optional_expression(variable.start.as_ref(), &mut names);
    }
    names.extend(block.error_signals.iter().map(ast::Identifier::as_str));
    for function in &block.protected_functions {
        collect_function(function, &mut names);
    }
    collect_method(&block.startup, &mut names);
    collect_method(&block.recalibrate, &mut names);
    collect_method(&block.do_step, &mut names);
    for function in &block.public_functions {
        collect_function(function, &mut names);
    }
    names.sort_unstable();
    names.dedup();
    names
}

fn collect_declaration<'a>(declaration: &'a ast::VariableDeclaration, names: &mut Vec<&'a str>) {
    names.push(declaration.name.lexeme());
    if let ast::TypeRef::Compartment(name) = &declaration.ty {
        names.push(name.lexeme());
    }
    for dimension in &declaration.dimensions {
        if let ast::Dimension::Expr(expression) = dimension {
            collect_expression(expression, names);
        }
    }
    collect_optional_expression(declaration.range.min.as_ref(), names);
    collect_optional_expression(declaration.range.max.as_ref(), names);
}

fn collect_method<'a>(method: &'a ast::BlockMethod, names: &mut Vec<&'a str>) {
    for declaration in &method.locals {
        collect_declaration(declaration, names);
    }
    collect_statements(&method.statements, names);
}

fn collect_function<'a>(function: &'a ast::UserFunction, names: &mut Vec<&'a str>) {
    names.push(function.name.lexeme());
    names.extend(function.signals.iter().map(ast::Identifier::as_str));
    for parameter in &function.parameters {
        collect_declaration(&parameter.decl, names);
    }
    for declaration in &function.locals {
        collect_declaration(declaration, names);
    }
    collect_statements(&function.statements, names);
}

fn collect_statements<'a>(
    statements: &'a [ast::Spanned<ast::Statement>],
    names: &mut Vec<&'a str>,
) {
    for statement in statements {
        collect_statement(&statement.node, names);
    }
}

fn collect_statement<'a>(statement: &'a ast::Statement, names: &mut Vec<&'a str>) {
    match statement {
        ast::Statement::Assignment { target, value } => {
            collect_reference(target, names);
            collect_expression(value, names);
        }
        ast::Statement::MultiAssignment { targets, call } => {
            for target in targets {
                collect_reference(target, names);
            }
            collect_call(call, names);
        }
        ast::Statement::Call(call) => collect_call(call, names),
        ast::Statement::If(statement) => collect_if_statement(statement, names),
        ast::Statement::For(statement) => collect_for(statement, names),
        ast::Statement::Limit(targets) => {
            for target in targets {
                if let ast::LimitTarget::Reference(reference) = target {
                    collect_reference(reference, names);
                }
            }
        }
        ast::Statement::Signal(signals) => {
            names.extend(signals.iter().map(ast::Identifier::as_str));
        }
    }
}

fn collect_if_statement<'a>(statement: &'a ast::IfStatement, names: &mut Vec<&'a str>) {
    for branch in &statement.branches {
        collect_condition(&branch.condition, names);
        collect_statements(&branch.body, names);
    }
    if let Some(statements) = &statement.else_body {
        collect_statements(statements, names);
    }
}

fn collect_condition<'a>(condition: &'a ast::Condition, names: &mut Vec<&'a str>) {
    match condition {
        ast::Condition::Expression(expression) => collect_expression(expression, names),
        ast::Condition::SignalCheck(check) => {
            if let Some(closure) = &check.closure {
                names.push(closure.as_str());
            }
            if let Some(test) = &check.test {
                names.extend(test.signals.iter().map(ast::Identifier::as_str));
            }
            collect_optional_expression(check.fallback.as_ref(), names);
        }
    }
}

fn collect_for<'a>(statement: &'a ast::ForLoop, names: &mut Vec<&'a str>) {
    if let Some(iterator) = &statement.iterator {
        names.push(iterator.lexeme());
    }
    collect_expression(&statement.start, names);
    collect_optional_expression(statement.step.as_ref(), names);
    collect_expression(&statement.stop, names);
    collect_statements(&statement.body, names);
}

fn collect_expression<'a>(expression: &'a ast::Expression, names: &mut Vec<&'a str>) {
    match expression {
        ast::Expression::Bool(_) | ast::Expression::Integer(_) | ast::Expression::Real(_) => {}
        ast::Expression::Ref(reference) | ast::Expression::Neg(reference) => {
            collect_reference(reference, names);
        }
        ast::Expression::Size { array, dimension } => {
            collect_reference(array, names);
            collect_expression(dimension, names);
        }
        ast::Expression::Call(call) => collect_call(call, names),
        ast::Expression::Paren(value) | ast::Expression::Not(value) => {
            collect_expression(value, names);
        }
        ast::Expression::If(value) => {
            for (condition, branch) in &value.branches {
                collect_expression(condition, names);
                collect_expression(branch, names);
            }
            collect_expression(&value.else_value, names);
        }
        ast::Expression::Array(values) => {
            for value in values {
                collect_expression(value, names);
            }
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_expression(lhs, names);
            collect_expression(rhs, names);
        }
    }
}

fn collect_call<'a>(call: &'a ast::FunctionCall, names: &mut Vec<&'a str>) {
    names.push(call.function.lexeme());
    for argument in &call.arguments {
        collect_expression(argument, names);
    }
}

fn collect_reference<'a>(reference: &'a ast::Reference, names: &mut Vec<&'a str>) {
    let parts = match reference {
        ast::Reference::Local(part) => std::slice::from_ref(part),
        ast::Reference::State(parts) => parts.as_slice(),
    };
    for part in parts {
        names.push(part.name.lexeme());
        for subscript in &part.subscripts {
            collect_expression(subscript, names);
        }
    }
}

fn collect_optional_expression<'a>(
    expression: Option<&'a ast::Expression>,
    names: &mut Vec<&'a str>,
) {
    if let Some(expression) = expression {
        collect_expression(expression, names);
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::Span;

    use super::*;

    fn declaration(name: &str) -> ast::VariableDeclaration {
        ast::VariableDeclaration::scalar(ast::ScalarType::Real, ast::Name::ident(name))
    }

    #[test]
    fn inventory_closes_over_functions_locals_calls_and_reference_parts() {
        let assignment = ast::Statement::Assignment {
            target: ast::Reference::State(vec![
                ast::RefPart::plain(ast::Name::ident("compartment")),
                ast::RefPart::plain(ast::Name::ident("field")),
            ]),
            value: ast::Expression::Call(ast::FunctionCall {
                function: ast::Name::ident("callee"),
                arguments: vec![ast::Expression::Ref(ast::Reference::local(
                    ast::Name::ident("temporary"),
                ))],
            }),
        };
        let function = ast::UserFunction {
            kind: ast::FunctionKind::Stateful,
            name: ast::Name::ident("controller"),
            signals: Vec::new(),
            parameters: vec![ast::Parameter {
                direction: ast::Direction::Input,
                decl: declaration("argument"),
            }],
            locals: vec![declaration("temporary")],
            statements: vec![ast::Spanned::new(assignment, Span::DUMMY)],
            span: Span::DUMMY,
        };
        let mut block = ast::Block::new(ast::Name::ident("Root"));
        block.public_functions.push(function);

        assert_eq!(
            collect(&block),
            [
                "Root",
                "argument",
                "callee",
                "compartment",
                "controller",
                "field",
                "temporary"
            ]
        );
    }
}
