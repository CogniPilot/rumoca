//! Position-based symbol navigation (hover + go-to-definition) over the GALEC
//! AST (SPEC_0034 D11).
//!
//! References live inside expressions as spanned [`crate::ast::RefPart`]s (M1),
//! and declarations carry their own span, so the symbol under a cursor is found
//! by span containment — no expression-level spans are required. Name
//! resolution reuses the validator's machinery ([`super::context`]): the same
//! scope rules that diagnose an unresolved reference locate its declaration.

use rumoca_core::Span;

use super::context::{
    BlockContext, Callee, FunctionScope, Resolved, lexeme, reference_parts, resolve, resolve_call,
};
use crate::ast::{
    Block, Condition, Expression, FunctionCall, LimitTarget, Reference, Statement, TypeRef,
    VariableDeclaration,
};

/// The symbol under a cursor: the span of the reference token itself (the hover
/// range), where it is declared (`None` for builtins — no source declaration),
/// and a one-line hover summary.
pub struct SymbolInfo {
    /// Span of the reference / call name under the cursor.
    pub reference_span: Span,
    /// Span of the declaration the reference resolves to (go-to-definition).
    pub definition_span: Option<Span>,
    /// Hover summary, e.g. `u : Real  (block variable)`.
    pub hover: String,
}

/// Resolve the symbol at a byte `offset`, or `None` when the cursor is not on a
/// resolvable reference or call target.
#[must_use]
pub fn symbol_at(block: &Block, offset: usize) -> Option<SymbolInfo> {
    let ctx = BlockContext::new(block);
    for body in ctx.bodies() {
        let mut scope = FunctionScope::new(&body);
        if let Some(info) = statements_at(&ctx, &mut scope, body.statements, offset) {
            return Some(info);
        }
    }
    None
}

fn statements_at<'a>(
    ctx: &BlockContext<'a>,
    scope: &mut FunctionScope<'a>,
    statements: &'a [crate::ast::Spanned<Statement>],
    offset: usize,
) -> Option<SymbolInfo> {
    for statement in statements {
        if let Some(info) = statement_at(ctx, scope, &statement.node, offset) {
            return Some(info);
        }
    }
    None
}

fn statement_at<'a>(
    ctx: &BlockContext<'a>,
    scope: &mut FunctionScope<'a>,
    statement: &'a Statement,
    offset: usize,
) -> Option<SymbolInfo> {
    match statement {
        Statement::Assignment { target, value } => reference_at(ctx, scope, target, offset)
            .or_else(|| expression_at(ctx, scope, value, offset)),
        Statement::MultiAssignment { targets, call } => targets
            .iter()
            .find_map(|target| reference_at(ctx, scope, target, offset))
            .or_else(|| call_at(ctx, scope, call, offset)),
        Statement::Call(call) => call_at(ctx, scope, call, offset),
        Statement::If(if_statement) => {
            for branch in &if_statement.branches {
                if let Some(info) = condition_at(ctx, scope, &branch.condition, offset) {
                    return Some(info);
                }
                if let Some(info) = statements_at(ctx, scope, &branch.body, offset) {
                    return Some(info);
                }
            }
            if_statement
                .else_body
                .as_deref()
                .and_then(|body| statements_at(ctx, scope, body, offset))
        }
        Statement::For(for_loop) => {
            // Loop bounds are evaluated in the enclosing scope.
            for bound in [
                Some(&for_loop.start),
                for_loop.step.as_ref(),
                Some(&for_loop.stop),
            ]
            .into_iter()
            .flatten()
            {
                if let Some(info) = expression_at(ctx, scope, bound, offset) {
                    return Some(info);
                }
            }
            // The iterator is in scope for the loop body.
            scope.push_iterator(for_loop.iterator.as_ref());
            let found = statements_at(ctx, scope, &for_loop.body, offset);
            scope.pop_iterator();
            found
        }
        Statement::Limit(targets) => targets.iter().find_map(|target| match target {
            LimitTarget::Reference(reference) => reference_at(ctx, scope, reference, offset),
            LimitTarget::SelfState => None,
        }),
        // Error-signal statements name signals, which carry no span in M1.
        Statement::Signal(_) => None,
    }
}

fn condition_at<'a>(
    ctx: &BlockContext<'a>,
    scope: &mut FunctionScope<'a>,
    condition: &'a Condition,
    offset: usize,
) -> Option<SymbolInfo> {
    match condition {
        Condition::Expression(expression) => expression_at(ctx, scope, expression, offset),
        // Signal checks: only the optional `or expr` fallback carries references.
        Condition::SignalCheck(check) => check
            .fallback
            .as_ref()
            .and_then(|fallback| expression_at(ctx, scope, fallback, offset)),
    }
}

fn expression_at<'a>(
    ctx: &BlockContext<'a>,
    scope: &mut FunctionScope<'a>,
    expression: &'a Expression,
    offset: usize,
) -> Option<SymbolInfo> {
    match expression {
        Expression::Ref(reference) | Expression::Neg(reference) => {
            reference_at(ctx, scope, reference, offset)
        }
        Expression::Size { array, dimension } => reference_at(ctx, scope, array, offset)
            .or_else(|| expression_at(ctx, scope, dimension, offset)),
        Expression::Call(call) => call_at(ctx, scope, call, offset),
        Expression::Paren(inner) | Expression::Not(inner) => {
            expression_at(ctx, scope, inner, offset)
        }
        Expression::If(if_expression) => {
            for (condition, value) in &if_expression.branches {
                if let Some(info) = expression_at(ctx, scope, condition, offset) {
                    return Some(info);
                }
                if let Some(info) = expression_at(ctx, scope, value, offset) {
                    return Some(info);
                }
            }
            expression_at(ctx, scope, &if_expression.else_value, offset)
        }
        Expression::Array(elements) => elements
            .iter()
            .find_map(|element| expression_at(ctx, scope, element, offset)),
        Expression::Binary { lhs, rhs, .. } => expression_at(ctx, scope, lhs, offset)
            .or_else(|| expression_at(ctx, scope, rhs, offset)),
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => None,
    }
}

/// A reference: check the subscript expressions first (a cursor on `i` in
/// `x[i]` resolves `i`, not `x`), then the reference name itself.
fn reference_at<'a>(
    ctx: &BlockContext<'a>,
    scope: &mut FunctionScope<'a>,
    reference: &'a Reference,
    offset: usize,
) -> Option<SymbolInfo> {
    let parts = reference_parts(reference);
    for part in parts {
        for subscript in &part.subscripts {
            if let Some(info) = expression_at(ctx, scope, subscript, offset) {
                return Some(info);
            }
        }
    }
    if !parts.iter().any(|part| span_contains(part.span, offset)) {
        return None;
    }
    let reference_span = union_spans(parts.iter().map(|part| part.span));
    resolve_reference(ctx, scope, reference, reference_span)
}

fn resolve_reference<'a>(
    ctx: &BlockContext<'a>,
    scope: &FunctionScope<'a>,
    reference: &'a Reference,
    reference_span: Span,
) -> Option<SymbolInfo> {
    let resolved = resolve(ctx, scope, reference).ok()?;
    let (definition_span, hover) = match resolved.target {
        Resolved::Entity { decl, .. } => {
            (non_dummy(decl.span), describe_var(decl, "block variable"))
        }
        Resolved::Component { decl } => {
            (non_dummy(decl.span), describe_var(decl, "state component"))
        }
        Resolved::Parameter(parameter) => (
            non_dummy(parameter.decl.span),
            describe_var(&parameter.decl, "parameter"),
        ),
        Resolved::Local(decl) => (non_dummy(decl.span), describe_var(decl, "local")),
        Resolved::Iterator => (None, "loop iterator : Integer".to_string()),
    };
    Some(SymbolInfo {
        reference_span,
        definition_span,
        hover,
    })
}

/// A call target: check the arguments first, then the function name.
fn call_at<'a>(
    ctx: &BlockContext<'a>,
    scope: &mut FunctionScope<'a>,
    call: &'a FunctionCall,
    offset: usize,
) -> Option<SymbolInfo> {
    for argument in &call.arguments {
        if let Some(info) = expression_at(ctx, scope, argument, offset) {
            return Some(info);
        }
    }
    if !span_contains(call.function.span(), offset) {
        return None;
    }
    let callee = resolve_call(ctx, &call.function)?;
    let (definition_span, hover) = match &callee {
        Callee::User(function) => (
            non_dummy(function.span),
            format!("{} `{}`", function.kind.keyword(), lexeme(&function.name)),
        ),
        Callee::Builtin(_) | Callee::Lifted { .. } => {
            (None, format!("builtin `{}`", callee.display_name()))
        }
    };
    Some(SymbolInfo {
        reference_span: call.function.span(),
        definition_span,
        hover,
    })
}

/// `name : Type  (kind)` — a one-line variable summary.
fn describe_var(decl: &VariableDeclaration, kind: &str) -> String {
    format!("{} : {}  ({kind})", lexeme(&decl.name), type_string(decl))
}

fn type_string(decl: &VariableDeclaration) -> String {
    let base = match &decl.ty {
        TypeRef::Primitive(scalar) => scalar.keyword().to_string(),
        TypeRef::Compartment(name) => lexeme(name),
    };
    if decl.dimensions.is_empty() {
        base
    } else {
        format!("{base}[{}]", decl.dimensions.len())
    }
}

fn span_contains(span: Span, offset: usize) -> bool {
    !span.is_dummy() && offset >= span.start.0 && offset < span.end.0
}

fn non_dummy(span: Span) -> Option<Span> {
    (!span.is_dummy()).then_some(span)
}

/// The smallest span covering all of `spans` (ignoring dummies).
fn union_spans(spans: impl Iterator<Item = Span>) -> Span {
    spans.fold(Span::DUMMY, |acc, span| {
        if acc.is_dummy() {
            span
        } else if span.is_dummy() {
            acc
        } else {
            Span::new(acc.source, acc.start.min(span.start), acc.end.max(span.end))
        }
    })
}
