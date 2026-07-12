//! Builders for statements & conditions (WI-4).
//!
//! Covers all seven [`crate::ast::Statement`] kinds, [`crate::ast::Condition`]
//! (LL(1) on a leading `signal`), and [`crate::ast::SignalCheck`], plus the
//! nested `if`/`for`/`limit` sub-productions. Every builder is
//! `TryFrom<&Generated> for AstType` with `type Error = anyhow::Error` (parol
//! 4.2.2 pins the child-conversion error type; see [`crate::parse::errors`]);
//! typed rejections are bridged with `GalecParseError::into_anyhow()`.

use crate::ast::{
    Condition, ForLoop, Identifier, IfBranch, IfStatement, LimitTarget, Reference, SignalCheck,
    SignalTest, Spanned, Statement,
};
use crate::parse::expr::call_args_to_vec;
use crate::parse::generated::galec_grammar_trait as g;
use crate::parse::refs::{
    computed_dimensions_to_vec, local_reference_ref_part, state_reference_tail_parts,
};
use crate::parse::span::{spanned_statement, statements_span};

// ---------------------------------------------------------------------------
// Statement
// ---------------------------------------------------------------------------

/// `statement : ( name_headed_statement | state_assignment | multi_assignment
///   | if_statement | for_loop | limit_statement | error_signal_statement ) ';'`.
impl TryFrom<&g::Statement> for Statement {
    type Error = anyhow::Error;

    fn try_from(ast: &g::Statement) -> Result<Self, Self::Error> {
        Ok(match &ast.statement_group {
            g::StatementGroup::NameHeadedStatement(s) => {
                name_headed_statement(&s.name_headed_statement)
            }
            g::StatementGroup::StateAssignment(s) => Self::Assignment {
                target: Reference::State(state_reference_tail_parts(
                    &s.state_assignment.state_reference.state_reference_tail,
                )),
                value: s.state_assignment.expression.clone(),
            },
            g::StatementGroup::MultiAssignment(s) => multi_assignment(&s.multi_assignment),
            g::StatementGroup::IfStatement(s) => Self::If(if_statement(&s.if_statement)),
            g::StatementGroup::ForLoop(s) => Self::For(for_loop(&s.for_loop)),
            g::StatementGroup::LimitStatement(s) => Self::Limit(limit_targets(&s.limit_statement)),
            g::StatementGroup::ErrorSignalStatement(s) => {
                Self::Signal(idents(&s.error_signal_statement))
            }
        })
    }
}

/// `name_headed_statement : name ( call_args | [ computed_dimensions ] ':=' expression )`.
///
/// A `call_args` tail is a bare [`Statement::Call`]; the `:=` alternative is a
/// [`Statement::Assignment`] onto a local reference (with optional subscripts).
fn name_headed_statement(ast: &g::NameHeadedStatement) -> Statement {
    let name = ast.name.clone();
    match &ast.name_headed_statement_group {
        g::NameHeadedStatementGroup::CallArgs(group) => Statement::Call(crate::ast::FunctionCall {
            function: name,
            arguments: call_args_to_vec(&group.call_args),
        }),
        g::NameHeadedStatementGroup::NameHeadedStatementOptColonEquExpression(group) => {
            let subscripts = match &group.name_headed_statement_opt {
                Some(opt) => computed_dimensions_to_vec(&opt.computed_dimensions),
                None => Vec::new(),
            };
            Statement::Assignment {
                target: Reference::Local(crate::ast::RefPart {
                    span: name.span(),
                    name,
                    subscripts,
                }),
                value: group.expression.clone(),
            }
        }
    }
}

/// `multi_assignment : '(' [ reference { ',' reference } ] ')' ':=' function_call`.
/// The target list may be empty (`() := f();`).
fn multi_assignment(ast: &g::MultiAssignment) -> Statement {
    let targets = match &ast.multi_assignment_opt {
        None => Vec::new(),
        Some(opt) => {
            let mut out = Vec::with_capacity(1 + opt.multi_assignment_opt_list.len());
            out.push(opt.reference.clone());
            for extra in &opt.multi_assignment_opt_list {
                out.push(extra.reference.clone());
            }
            out
        }
    };
    Statement::MultiAssignment {
        targets,
        call: ast.function_call.clone(),
    }
}

// ---------------------------------------------------------------------------
// if / for / limit
// ---------------------------------------------------------------------------

/// `if_statement : 'if' condition 'then' { stmt }
///   { 'elseif' condition 'then' { stmt } } [ 'else' { stmt } ] 'end' 'if'`.
fn if_statement(ast: &g::IfStatement) -> IfStatement {
    let mut branches = Vec::with_capacity(1 + ast.if_statement_list0.len());
    branches.push(if_branch(
        ast.condition.clone(),
        statements(ast.if_statement_list.iter().map(|s| &s.statement)),
    ));
    for elseif in &ast.if_statement_list0 {
        branches.push(if_branch(
            elseif.condition.clone(),
            statements(elseif.if_statement_list0_list.iter().map(|s| &s.statement)),
        ));
    }
    let else_body = ast
        .if_statement_opt
        .as_ref()
        .map(|opt| statements(opt.if_statement_opt_list.iter().map(|s| &s.statement)));
    IfStatement {
        branches,
        else_body,
    }
}

/// `for_loop : 'for' bounded_iteration 'loop' { stmt } 'end' 'for'`.
///
/// `bounded_iteration : [ name 'in' ] expr ':' expr [ ':' expr ]`. Two operands
/// mean `start:stop` (`step: None`); three mean `start:step:stop`.
fn for_loop(ast: &g::ForLoop) -> ForLoop {
    let iteration = &ast.bounded_iteration;
    let iterator = iteration
        .bounded_iteration_opt
        .as_ref()
        .map(|o| o.name.clone());
    let (step, stop) = match &iteration.bounded_iteration_opt0 {
        None => (None, iteration.expression0.clone()),
        Some(third) => (
            Some(iteration.expression0.clone()),
            third.expression.clone(),
        ),
    };
    ForLoop {
        iterator,
        start: iteration.expression.clone(),
        step,
        stop,
        body: statements(ast.for_loop_list.iter().map(|s| &s.statement)),
    }
}

/// `limit_statement : 'limit' limit_target { ',' limit_target }`.
fn limit_targets(ast: &g::LimitStatement) -> Vec<LimitTarget> {
    let mut out = Vec::with_capacity(1 + ast.limit_statement_list.len());
    out.push(limit_target(&ast.limit_target));
    for extra in &ast.limit_statement_list {
        out.push(limit_target(&extra.limit_target));
    }
    out
}

/// `limit_target : 'self' [ state_reference_tail ] | local_reference`.
///
/// Bare `self` limits every ranged state (`LimitTarget::SelfState`); `self.x`
/// is a state reference and `x` a local reference.
fn limit_target(ast: &g::LimitTarget) -> LimitTarget {
    match ast {
        g::LimitTarget::SelfLimitTargetOpt(t) => match &t.limit_target_opt {
            None => LimitTarget::SelfState,
            Some(opt) => LimitTarget::Reference(Reference::State(state_reference_tail_parts(
                &opt.state_reference_tail,
            ))),
        },
        g::LimitTarget::LocalReference(t) => LimitTarget::Reference(Reference::Local(
            local_reference_ref_part(&t.local_reference),
        )),
    }
}

// ---------------------------------------------------------------------------
// condition & signal-check
// ---------------------------------------------------------------------------

/// `condition : error_signal_check | expression` (LL(1) on a leading `signal`).
impl TryFrom<&g::Condition> for Condition {
    type Error = anyhow::Error;

    fn try_from(ast: &g::Condition) -> Result<Self, Self::Error> {
        Ok(match ast {
            g::Condition::ErrorSignalCheck(c) => Self::SignalCheck(c.error_signal_check.clone()),
            g::Condition::Expression(c) => Self::Expression(c.expression.clone()),
        })
    }
}

/// `error_signal_check : 'signal' [ ident ] [ [ 'not' ] 'in' ident { ',' ident } ]
///   [ 'or' expression ]` — closure, test set, and fallback are each optional.
impl TryFrom<&g::ErrorSignalCheck> for SignalCheck {
    type Error = anyhow::Error;

    fn try_from(ast: &g::ErrorSignalCheck) -> Result<Self, Self::Error> {
        let closure = ast
            .error_signal_check_opt
            .as_ref()
            .map(|o| Identifier::new(o.ident.ident.text()));
        let test = ast.error_signal_check_opt0.as_ref().map(|test| {
            let mut signals = Vec::with_capacity(1 + test.error_signal_check_opt0_list.len());
            signals.push(Identifier::new(test.ident.ident.text()));
            for extra in &test.error_signal_check_opt0_list {
                signals.push(Identifier::new(extra.ident.ident.text()));
            }
            SignalTest {
                negated: test.error_signal_check_opt2.is_some(),
                signals,
            }
        });
        let fallback = ast
            .error_signal_check_opt1
            .as_ref()
            .map(|o| o.expression.clone());
        Ok(Self {
            closure,
            test,
            fallback,
        })
    }
}

// ---------------------------------------------------------------------------
// Shared helpers over non-`%nt_type` repetition wrappers
// ---------------------------------------------------------------------------

/// Collect a spanned statement list from any generated repetition wrapper whose
/// element exposes an already-converted `statement` field, reconstructing each
/// statement's span from its spanned children (D11).
fn statements<'a>(
    items: impl Iterator<Item = &'a crate::ast::Statement>,
) -> Vec<Spanned<Statement>> {
    items.map(spanned_statement).collect()
}

/// Build an [`IfBranch`] whose span is the union of its body statements (the
/// condition is an expression / signal-check, which carry no span in M1).
fn if_branch(condition: Condition, body: Vec<Spanned<Statement>>) -> IfBranch {
    IfBranch {
        span: statements_span(&body),
        condition,
        body,
    }
}

/// `error_signal_statement : 'signal' ident { ',' ident }` → the non-empty
/// identifier list (also shared shape used by the block error-signal decls).
fn idents(ast: &g::ErrorSignalStatement) -> Vec<Identifier> {
    let mut out = Vec::with_capacity(1 + ast.error_signal_statement_list.len());
    out.push(Identifier::new(ast.ident.ident.text()));
    for extra in &ast.error_signal_statement_list {
        out.push(Identifier::new(extra.ident.ident.text()));
    }
    out
}
