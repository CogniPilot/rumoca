//! Proven lexical placement of generated-C automatic locals (GAL-034).

use std::collections::{HashMap, HashSet};

use rumoca_ir_galec::ast;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum ScopeStep {
    IfBranch { statement: usize, branch: usize },
    IfElse { statement: usize },
    ForBody { statement: usize },
}

pub(super) type ScopePath = Vec<ScopeStep>;

pub(super) struct LocalPlacements<'a> {
    by_scope: HashMap<ScopePath, Vec<&'a ast::VariableDeclaration>>,
}

impl<'a> LocalPlacements<'a> {
    pub(super) fn derive(
        declarations: &'a [ast::VariableDeclaration],
        statements: &'a [ast::Spanned<ast::Statement>],
    ) -> Self {
        let names = declarations
            .iter()
            .map(|declaration| declaration.name.lexeme())
            .collect::<HashSet<_>>();
        let mut uses = HashMap::<&str, ScopePath>::new();
        collect_statements(statements, &mut Vec::new(), &names, &mut uses);
        let mut by_scope = HashMap::<ScopePath, Vec<_>>::new();
        for declaration in declarations {
            if let Some(path) = uses.remove(declaration.name.lexeme()) {
                by_scope.entry(path).or_default().push(declaration);
            }
        }
        Self { by_scope }
    }

    pub(super) fn at(&self, path: &[ScopeStep]) -> Vec<&'a ast::VariableDeclaration> {
        self.by_scope.get(path).cloned().unwrap_or_default()
    }
}

fn collect_statements<'a>(
    statements: &'a [ast::Spanned<ast::Statement>],
    path: &mut ScopePath,
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
) {
    for (statement_index, statement) in statements.iter().enumerate() {
        collect_statement(&statement.node, statement_index, path, names, uses);
    }
}

fn collect_statement<'a>(
    statement: &'a ast::Statement,
    statement_index: usize,
    path: &mut ScopePath,
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
) {
    match statement {
        ast::Statement::Assignment { target, value } => {
            collect_reference(target, path, names, uses);
            collect_expression(value, path, names, uses);
        }
        ast::Statement::MultiAssignment { targets, call } => {
            for target in targets {
                collect_reference(target, path, names, uses);
            }
            collect_call(call, path, names, uses);
        }
        ast::Statement::Call(call) => collect_call(call, path, names, uses),
        ast::Statement::If(value) => {
            for (branch_index, branch) in value.branches.iter().enumerate() {
                collect_condition(&branch.condition, path, names, uses);
                path.push(ScopeStep::IfBranch {
                    statement: statement_index,
                    branch: branch_index,
                });
                collect_statements(&branch.body, path, names, uses);
                path.pop();
            }
            if let Some(body) = &value.else_body {
                path.push(ScopeStep::IfElse {
                    statement: statement_index,
                });
                collect_statements(body, path, names, uses);
                path.pop();
            }
        }
        ast::Statement::For(value) => {
            collect_expression(&value.start, path, names, uses);
            if let Some(step) = &value.step {
                collect_expression(step, path, names, uses);
            }
            collect_expression(&value.stop, path, names, uses);
            path.push(ScopeStep::ForBody {
                statement: statement_index,
            });
            let mut body_names = names.clone();
            if let Some(iterator) = &value.iterator {
                body_names.remove(iterator.lexeme());
            }
            collect_statements(&value.body, path, &body_names, uses);
            path.pop();
        }
        ast::Statement::Limit(targets) => {
            for target in targets {
                if let ast::LimitTarget::Reference(reference) = target {
                    collect_reference(reference, path, names, uses);
                }
            }
        }
        ast::Statement::Signal(_) => {}
    }
}

fn collect_condition<'a>(
    condition: &'a ast::Condition,
    path: &[ScopeStep],
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
) {
    match condition {
        ast::Condition::Expression(expression) => collect_expression(expression, path, names, uses),
        ast::Condition::SignalCheck(check) => {
            if let Some(closure) = &check.closure
                && names.contains(closure.as_str())
            {
                record_use(uses, closure.as_str(), path);
            }
            if let Some(fallback) = &check.fallback {
                collect_expression(fallback, path, names, uses);
            }
        }
    }
}

fn collect_call<'a>(
    call: &'a ast::FunctionCall,
    path: &[ScopeStep],
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
) {
    for argument in &call.arguments {
        collect_expression(argument, path, names, uses);
    }
}

fn collect_expression<'a>(
    expression: &'a ast::Expression,
    path: &[ScopeStep],
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
) {
    match expression {
        ast::Expression::Bool(_) | ast::Expression::Integer(_) | ast::Expression::Real(_) => {}
        ast::Expression::Ref(reference) | ast::Expression::Neg(reference) => {
            collect_reference(reference, path, names, uses);
        }
        ast::Expression::Size { array, dimension } => {
            collect_reference(array, path, names, uses);
            collect_expression(dimension, path, names, uses);
        }
        ast::Expression::Call(call) => collect_call(call, path, names, uses),
        ast::Expression::Paren(value) | ast::Expression::Not(value) => {
            collect_expression(value, path, names, uses);
        }
        ast::Expression::If(value) => {
            for (condition, branch) in &value.branches {
                collect_expression(condition, path, names, uses);
                collect_expression(branch, path, names, uses);
            }
            collect_expression(&value.else_value, path, names, uses);
        }
        ast::Expression::Array(values) => {
            for value in values {
                collect_expression(value, path, names, uses);
            }
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_expression(lhs, path, names, uses);
            collect_expression(rhs, path, names, uses);
        }
    }
}

fn collect_reference<'a>(
    reference: &'a ast::Reference,
    path: &[ScopeStep],
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
) {
    let parts = match reference {
        ast::Reference::Local(part) => {
            if names.contains(part.name.lexeme()) {
                record_use(uses, part.name.lexeme(), path);
            }
            std::slice::from_ref(part)
        }
        ast::Reference::State(parts) => parts.as_slice(),
    };
    for subscript in parts.iter().flat_map(|part| &part.subscripts) {
        collect_expression(subscript, path, names, uses);
    }
}

fn record_use<'a>(uses: &mut HashMap<&'a str, ScopePath>, name: &'a str, path: &[ScopeStep]) {
    uses.entry(name)
        .and_modify(|owner| {
            let common = owner
                .iter()
                .zip(path)
                .take_while(|(lhs, rhs)| lhs == rhs)
                .count();
            owner.truncate(common);
        })
        .or_insert_with(|| path.to_vec());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn declaration(name: &str) -> ast::VariableDeclaration {
        ast::VariableDeclaration::scalar(ast::ScalarType::Real, ast::Name::ident(name))
    }

    fn assignment(target: &str, source: &str) -> ast::Spanned<ast::Statement> {
        ast::Spanned::dummy(ast::Statement::Assignment {
            target: ast::Reference::local(ast::Name::ident(target)),
            value: ast::Expression::Ref(ast::Reference::local(ast::Name::ident(source))),
        })
    }

    fn names<'a>(values: &'a [&'a ast::VariableDeclaration]) -> Vec<&'a str> {
        values.iter().map(|value| value.name.lexeme()).collect()
    }

    #[test]
    fn branch_local_and_cross_scope_uses_have_distinct_owners() {
        let declarations = vec![
            declaration("branch_value"),
            declaration("shared_value"),
            declaration("sink"),
        ];
        let statements = vec![
            ast::Spanned::dummy(ast::Statement::If(ast::IfStatement {
                branches: vec![ast::IfBranch {
                    condition: ast::Condition::Expression(ast::Expression::Bool(true)),
                    body: vec![
                        assignment("branch_value", "branch_value"),
                        assignment("shared_value", "branch_value"),
                    ],
                    span: rumoca_core::Span::DUMMY,
                }],
                else_body: None,
            })),
            assignment("sink", "shared_value"),
        ];
        let placements = LocalPlacements::derive(&declarations, &statements);

        assert_eq!(names(&placements.at(&[])), ["shared_value", "sink"]);
        assert_eq!(
            names(&placements.at(&[ScopeStep::IfBranch {
                statement: 0,
                branch: 0,
            }])),
            ["branch_value"]
        );
    }

    #[test]
    fn loop_only_local_is_owned_by_the_loop_body() {
        let declarations = vec![declaration("loop_value")];
        let statements = vec![ast::Spanned::dummy(ast::Statement::For(ast::ForLoop {
            iterator: Some(ast::Name::ident("index")),
            start: ast::Expression::Integer(1),
            step: None,
            stop: ast::Expression::Integer(2),
            body: vec![assignment("loop_value", "loop_value")],
        }))];
        let placements = LocalPlacements::derive(&declarations, &statements);

        assert!(placements.at(&[]).is_empty());
        assert_eq!(
            names(&placements.at(&[ScopeStep::ForBody { statement: 0 }])),
            ["loop_value"]
        );
    }

    #[test]
    fn unreferenced_checked_local_needs_no_c_storage() {
        let declarations = vec![declaration("lowered_away")];
        let placements = LocalPlacements::derive(&declarations, &[]);

        assert!(placements.at(&[]).is_empty());
    }
}
