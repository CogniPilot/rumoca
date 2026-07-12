//! Span-bubbling helpers for the builders (SPEC_0034 D11).
//!
//! Only `Ident`/literal terminals retain a token through parol's `%nt_type`
//! conversion, so every mid-level node's span is reconstructed here as the
//! union of its already-converted children's spans. A `DUMMY` operand is
//! ignored, so a node whose children are only partially spanned (e.g. a
//! statement whose value expression is not yet spanned, M1) still yields the
//! tightest span its spanned children provide, never a bogus zero span.

use rumoca_core::Span;

use crate::ast::{LimitTarget, Name, Reference, Spanned, Statement};

/// The smallest span covering both operands. A `DUMMY` operand is ignored (the
/// other wins); two `DUMMY`s yield `DUMMY`. Operands are assumed to share one
/// source (all tokens of a single parse do).
pub(crate) fn union(a: Span, b: Span) -> Span {
    if a.is_dummy() {
        return b;
    }
    if b.is_dummy() {
        return a;
    }
    Span::new(a.source, a.start.min(b.start), a.end.max(b.end))
}

/// Union of a sequence of spans (`DUMMY` when empty or all-dummy).
pub(crate) fn union_all(spans: impl IntoIterator<Item = Span>) -> Span {
    spans.into_iter().fold(Span::DUMMY, union)
}

/// The span covering a whole reference: the union of its parts.
pub(crate) fn reference_span(reference: &Reference) -> Span {
    match reference {
        Reference::Local(part) => part.span,
        Reference::State(parts) => union_all(parts.iter().map(|part| part.span)),
    }
}

/// The span covering a spanned-statement list: the union of its items.
pub(crate) fn statements_span(statements: &[Spanned<Statement>]) -> Span {
    union_all(statements.iter().map(|statement| statement.span))
}

/// Wrap an already-converted statement with its reconstructed span (D11).
pub(crate) fn spanned_statement(statement: &Statement) -> Spanned<Statement> {
    Spanned::new(statement.clone(), statement_span(statement))
}

/// Reconstruct a statement's span from its spanned children (references and
/// call-target names). Expressions and error signals carry no span in M1, so a
/// statement dominated by those (e.g. `signal s;`) yields `DUMMY` — a valid
/// "no precise location yet", never a bogus zero span.
pub(crate) fn statement_span(statement: &Statement) -> Span {
    match statement {
        Statement::Assignment { target, .. } => reference_span(target),
        Statement::MultiAssignment { targets, call } => union(
            union_all(targets.iter().map(reference_span)),
            call.function.span(),
        ),
        Statement::Call(call) => call.function.span(),
        Statement::If(if_statement) => union(
            union_all(if_statement.branches.iter().map(|branch| branch.span)),
            if_statement
                .else_body
                .as_deref()
                .map_or(Span::DUMMY, statements_span),
        ),
        Statement::For(for_loop) => union(
            for_loop.iterator.as_ref().map_or(Span::DUMMY, Name::span),
            statements_span(&for_loop.body),
        ),
        Statement::Limit(targets) => union_all(targets.iter().filter_map(|target| match target {
            LimitTarget::Reference(reference) => Some(reference_span(reference)),
            LimitTarget::SelfState => None,
        })),
        // `Identifier` carries no span in M1, so error-signal statements have no
        // precise span yet (their diagnostics fall back to the structural path).
        Statement::Signal(_) => Span::DUMMY,
    }
}
