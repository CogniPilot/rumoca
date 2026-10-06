use rumoca_core::{ComponentReference, ForIndex, Span, Statement};

use super::{Branch, GuardVisitor, NormalizedStatement as N};

/// Structural observation exposes every dependency, target and lexical loop.
/// Path-sensitive dataflow uses the same recursive statement inventory.
pub trait StatementVisitor<'locals>: GuardVisitor<'locals> {
    fn visit_source_target(&mut self, target: &ComponentReference);
    fn enter_loop(&mut self, _span: Span) {}
    fn exit_loop(&mut self, _span: Span) {}
    fn enter_for_index(&mut self, _index: &ForIndex) {}
    fn exit_for_indices(&mut self, _indices: &[ForIndex]) {}
}

impl<'locals> N<'locals> {
    pub fn visit<V: StatementVisitor<'locals>>(&self, visitor: &mut V) {
        match self {
            Self::Source(source) => visit_source(source.source(), visitor),
            Self::Definition(definition) => definition.visit(visitor),
            Self::For {
                indices,
                statements,
                span,
            } => {
                visitor.enter_loop(*span);
                for index in indices {
                    visitor.visit_expression(&index.range);
                    visitor.enter_for_index(index);
                }
                visit_sequence(statements, visitor);
                visitor.exit_for_indices(indices);
                visitor.exit_loop(*span);
            }
            Self::While {
                condition,
                statements,
                span,
            } => {
                visitor.enter_loop(*span);
                condition.visit(visitor);
                visit_sequence(statements, visitor);
                visitor.exit_loop(*span);
            }
            Self::If {
                branches, fallback, ..
            } => {
                visit_branches(branches, visitor);
                if let Some(fallback) = fallback {
                    visit_sequence(fallback, visitor);
                }
            }
            Self::When { branches, .. } => visit_branches(branches, visitor),
        }
    }
}

fn visit_sequence<'locals, V: StatementVisitor<'locals>>(
    statements: &[N<'locals>],
    visitor: &mut V,
) {
    for statement in statements {
        statement.visit(visitor);
    }
}

fn visit_branches<'locals, V: StatementVisitor<'locals>>(
    branches: &[Branch<'locals>],
    visitor: &mut V,
) {
    for branch in branches {
        branch.condition.visit(visitor);
        visit_sequence(&branch.statements, visitor);
    }
}

fn visit_target_indices<'locals, V: StatementVisitor<'locals>>(
    target: &ComponentReference,
    visitor: &mut V,
) {
    for part in target.parts() {
        for subscript in &part.subs {
            visitor.visit_subscript(subscript);
        }
    }
}

fn visit_source<'locals, V: StatementVisitor<'locals>>(source: &Statement, visitor: &mut V) {
    match source {
        Statement::Assignment { comp, value, .. }
        | Statement::Reinit {
            variable: comp,
            value,
            ..
        } => {
            visitor.visit_expression(value);
            visit_target_indices(comp, visitor);
            visitor.visit_source_target(comp);
        }
        Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                visitor.visit_expression(arg);
            }
            for output in outputs.iter().flatten() {
                visit_target_indices(output, visitor);
            }
            for output in outputs.iter().flatten() {
                visitor.visit_source_target(output);
            }
        }
        Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            visitor.visit_expression(condition);
            visitor.visit_expression(message);
            if let Some(level) = level {
                visitor.visit_expression(level);
            }
        }
        Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => {}
        Statement::For { .. }
        | Statement::While { .. }
        | Statement::If { .. }
        | Statement::When { .. } => {
            unreachable!("checked SourceStatement cannot conceal control flow")
        }
    }
}
