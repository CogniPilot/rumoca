//! Resolve a diagnostic's structural [`Location`] to the source [`Span`] of the
//! node it points at (SPEC_0034 D11).
//!
//! Validator diagnostics carry a structural AST *path* (block / method /
//! statement-index / branch …), not a source span. Rather than thread a span
//! into every diagnostic construction site, [`span_of`] walks that path over the
//! (now span-carrying) AST and returns the span M1 attached to the node the path
//! names. This positions validator diagnostics for the LSP and CLI with zero
//! changes to the analyses, and its precision tracks the path's precision: a
//! method-granular path yields the method span, a statement-granular path the
//! statement span. It returns the *deepest* real (non-`DUMMY`) span the path
//! reaches, or `None` when nothing along the path carries one.

use rumoca_core::Span;

use super::context::lexeme;
use super::spans::non_dummy;
use crate::ast::{
    Block, BlockMethod, BlockMethodKind, Parameter, Spanned, StateCompartment, Statement,
    UserFunction, VariableDeclaration,
};
use crate::diagnostic::{Location, PathSegment};

/// The source span of the node a diagnostic [`Location`] points at, or `None`
/// when neither the target nor any ancestor along its path carries a real span.
#[must_use]
pub fn span_of(block: &Block, location: &Location) -> Option<Span> {
    // `best` is the deepest real span resolved so far; each descent that lands a
    // real span replaces it (deeper == more precise), a `DUMMY` keeps the last.
    let mut best = non_dummy(block.span);
    let mut statements: Option<&[Spanned<Statement>]> = None;
    let mut locals: &[VariableDeclaration] = &[];
    let mut params: &[Parameter] = &[];
    let mut compartment: Option<&StateCompartment> = None;
    // The most recently resolved statement; a following Branch/Else descends
    // into it (a for-loop body is entered eagerly, see Statement below).
    let mut current: Option<&Statement> = None;

    for segment in &location.path {
        match segment {
            // Root: `block.span` already seeded `best`.
            PathSegment::Block(_) => {}
            PathSegment::Method(kind) => {
                let method = block_method(block, *kind);
                best = keep(method.span, best);
                statements = Some(&method.statements);
                locals = &method.locals;
                params = &[];
                current = None;
            }
            PathSegment::Function(name) => {
                if let Some(function) = find_function(block, name) {
                    best = keep(function.span, best);
                    statements = Some(&function.statements);
                    locals = &function.locals;
                    params = &function.parameters;
                    current = None;
                }
            }
            PathSegment::Compartment(name) => {
                compartment = block.compartments.iter().find(|c| lexeme(&c.name) == *name);
                if let Some(found) = compartment {
                    best = keep(found.span, best);
                }
            }
            PathSegment::Variable(name) => {
                if let Some(span) = variable_span(block, compartment, name) {
                    best = keep(span, best);
                }
            }
            PathSegment::Parameter(name) => {
                if let Some(parameter) = params.iter().find(|p| lexeme(&p.decl.name) == *name) {
                    best = keep(parameter.decl.span, best);
                }
            }
            PathSegment::Local(name) => {
                if let Some(local) = locals.iter().find(|d| lexeme(&d.name) == *name) {
                    best = keep(local.span, best);
                }
            }
            PathSegment::Statement(index) => {
                let Some(statement) = statements.and_then(|list| list.get(*index)) else {
                    current = None;
                    continue;
                };
                best = keep(statement.span, best);
                current = Some(&statement.node);
                // A for-loop body carries no intermediate path segment, so enter
                // it now: a following Statement indexes the loop body.
                if let Statement::For(for_loop) = &statement.node {
                    statements = Some(&for_loop.body);
                }
            }
            PathSegment::Branch(index) => {
                if let Some(Statement::If(if_statement)) = current
                    && let Some(branch) = if_statement.branches.get(*index)
                {
                    best = keep(branch.span, best);
                    statements = Some(&branch.body);
                }
                current = None;
            }
            PathSegment::Else => {
                if let Some(Statement::If(if_statement)) = current
                    && let Some(else_body) = &if_statement.else_body
                {
                    statements = Some(else_body);
                }
                current = None;
            }
            // Conditions are unspanned in M1; keep the enclosing branch/statement
            // span already in `best`.
            PathSegment::Condition => {}
        }
    }
    best
}

/// The block-interface method for a kind.
fn block_method(block: &Block, kind: BlockMethodKind) -> &BlockMethod {
    match kind {
        BlockMethodKind::Startup => &block.startup,
        BlockMethodKind::Recalibrate => &block.recalibrate,
        BlockMethodKind::DoStep => &block.do_step,
    }
}

/// A user function by lexeme, protected then public (declaration order).
fn find_function<'a>(block: &'a Block, name: &str) -> Option<&'a UserFunction> {
    block
        .protected_functions
        .iter()
        .chain(&block.public_functions)
        .find(|function| lexeme(&function.name) == name)
}

/// The declaration span of a named variable: a compartment entity (when a
/// compartment is in scope), else a block interface variable or protected
/// entity. `None` if unresolved or only a `DUMMY` span is available.
fn variable_span(
    block: &Block,
    compartment: Option<&StateCompartment>,
    name: &str,
) -> Option<Span> {
    if let Some(found) = compartment
        && let Some(entity) = found.entities.iter().find(|e| lexeme(&e.decl.name) == name)
    {
        return non_dummy(entity.decl.span);
    }
    if let Some(variable) = block
        .interface
        .iter()
        .find(|v| lexeme(&v.decl.name) == name)
    {
        return non_dummy(variable.decl.span);
    }
    if let Some(entity) = block
        .protected
        .iter()
        .find(|e| lexeme(&e.decl.name) == name)
    {
        return non_dummy(entity.decl.span);
    }
    None
}

/// Keep the deeper real span: `span` when real, else the prior `best`.
fn keep(span: Span, best: Option<Span>) -> Option<Span> {
    non_dummy(span).or(best)
}
