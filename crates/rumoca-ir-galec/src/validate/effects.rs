//! Side-effect analysis (S-2.3 plus the §3.2.4 writability and
//! stateful-call-isolation rules, unnumbered `S-TODO`s in Beta-1; trap
//! T12): stateless functions never write state and never call stateful
//! functions (direct detection is program-complete: any violating
//! transitive chain contains a stateless->stateful direct edge); a stateful
//! call has no sibling function-calls or state-references within the same
//! expression (argument evaluation order is undefined); if-expressions
//! contain no stateful calls at all; control-inputs, input parameters, and
//! loop iterators are read-only. `limit` saturates — i.e. writes — its
//! targets (trap T3), so the write rules cover limit targets too.

use crate::ast::{
    Condition, Expression, FunctionCall, FunctionKind, IfStatement, LimitTarget, Reference,
    Spanned, Statement,
};
use crate::diagnostic::{GalecError, PathSegment};

use super::context::{
    BlockContext, Cursor, EntityKind, FunctionScope, Resolved, lexeme, reference_lexeme, resolve,
    resolve_call,
};

pub(super) fn check(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    for body in ctx.bodies() {
        let mut checker = EffectChecker {
            ctx,
            scope: FunctionScope::new(&body),
            cursor: Cursor::for_body(ctx, &body),
            stateless: body.kind == FunctionKind::Stateless,
            diags,
        };
        checker.statements(body.statements);
    }
}

/// What an expression subtree contains, for the sibling-isolation rule.
#[derive(Clone, Default)]
struct Summary {
    call: bool,
    /// Name of a stateful call in the subtree, if any.
    stateful: Option<String>,
    state_ref: bool,
}

impl Summary {
    fn merge(self, other: Self) -> Self {
        Self {
            call: self.call || other.call,
            stateful: self.stateful.or(other.stateful),
            state_ref: self.state_ref || other.state_ref,
        }
    }
}

struct EffectChecker<'a, 'd> {
    ctx: &'a BlockContext<'a>,
    scope: FunctionScope<'a>,
    cursor: Cursor,
    stateless: bool,
    diags: &'d mut Vec<GalecError>,
}

impl<'a> EffectChecker<'a, '_> {
    fn statements(&mut self, statements: &'a [Spanned<Statement>]) {
        for (index, statement) in statements.iter().enumerate() {
            self.cursor.push(PathSegment::Statement(index));
            self.statement(&statement.node);
            self.cursor.pop();
        }
    }

    fn statement(&mut self, statement: &'a Statement) {
        match statement {
            Statement::Assignment { target, value } => {
                self.write_target(target);
                self.summarize(value);
            }
            Statement::MultiAssignment { targets, call } => {
                for target in targets {
                    self.write_target(target);
                }
                self.call_summary(call);
            }
            Statement::Call(call) => {
                self.call_summary(call);
            }
            Statement::If(if_statement) => self.if_statement(if_statement),
            Statement::For(for_loop) => {
                self.scope.push_iterator(for_loop.iterator.as_ref());
                self.statements(&for_loop.body);
                self.scope.pop_iterator();
            }
            Statement::Limit(targets) => self.limit(targets),
            Statement::Signal(_) => {}
        }
    }

    /// `limit` saturates (writes) its targets (trap T3), so the write rules
    /// apply: stateless functions may not limit state, and read-only
    /// entities may not be limited. `limit self;` saturates the whole
    /// control-state. (Component targets are legal and valueless — ignored
    /// by [`Self::write_target`].)
    fn limit(&mut self, targets: &'a [LimitTarget]) {
        for target in targets {
            match target {
                LimitTarget::Reference(reference) => self.write_target(reference),
                LimitTarget::SelfState => self.limit_self(),
            }
        }
    }

    /// `limit self;` saturates the whole control-state — illegal in a
    /// stateless function.
    fn limit_self(&mut self) {
        if self.stateless {
            self.diags.push(GalecError::StatelessWritesState {
                location: self.cursor.here(),
                target: "self".to_string(),
            });
        }
    }

    fn if_statement(&mut self, if_statement: &'a IfStatement) {
        for (index, branch) in if_statement.branches.iter().enumerate() {
            self.cursor.push(PathSegment::Branch(index));
            self.condition(&branch.condition);
            self.statements(&branch.body);
            self.cursor.pop();
        }
        if let Some(else_body) = &if_statement.else_body {
            self.cursor.push(PathSegment::Else);
            self.statements(else_body);
            self.cursor.pop();
        }
    }

    fn condition(&mut self, condition: &'a Condition) {
        match condition {
            Condition::Expression(expression) => {
                self.summarize(expression);
            }
            Condition::SignalCheck(check) => {
                if let Some(fallback) = &check.fallback {
                    self.summarize(fallback);
                }
            }
        }
    }

    /// Writability: stateless functions never write state; control-inputs,
    /// input parameters, and loop iterators are read-only everywhere.
    /// (Component-typed targets are already rejected by the type analysis.)
    fn write_target(&mut self, target: &'a Reference) {
        let resolved = match resolve(self.ctx, &self.scope, target) {
            Ok(resolved) => resolved.target,
            // Reported by the type analysis.
            Err(_) => return,
        };
        match resolved {
            Resolved::Entity { root_kind, .. } => {
                if self.stateless {
                    self.diags.push(GalecError::StatelessWritesState {
                        location: self.cursor.here(),
                        target: reference_lexeme(target),
                    });
                }
                if root_kind == EntityKind::Input {
                    self.read_only("control-input", target);
                }
            }
            Resolved::Parameter(parameter) => {
                if parameter.direction == crate::ast::Direction::Input {
                    self.read_only("input parameter", target);
                }
            }
            Resolved::Iterator => self.read_only("loop iterator", target),
            Resolved::Component { .. } | Resolved::Local(_) => {}
        }
    }

    fn read_only(&mut self, kind: &'static str, target: &Reference) {
        self.diags.push(GalecError::WriteToReadOnly {
            location: self.cursor.here(),
            kind,
            name: reference_lexeme(target),
        });
    }

    // -----------------------------------------------------------------
    // Stateful-call isolation (sibling rule) and if-expression ban
    // -----------------------------------------------------------------

    /// Summarize an expression bottom-up, reporting the sibling-isolation
    /// rule at every node with two or more operand subtrees.
    fn summarize(&mut self, expression: &'a Expression) -> Summary {
        match expression {
            Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {
                Summary::default()
            }
            Expression::Ref(reference) | Expression::Neg(reference) => Summary {
                state_ref: matches!(reference, Reference::State(_)),
                ..Summary::default()
            },
            // Subscripts and size() dimensions are static (builtin-only,
            // stateless), so they cannot carry effects.
            Expression::Size { array, .. } => Summary {
                state_ref: matches!(array, Reference::State(_)),
                ..Summary::default()
            },
            Expression::Call(call) => self.call_summary(call),
            Expression::Paren(inner) | Expression::Not(inner) => self.summarize(inner),
            Expression::If(if_expression) => self.if_expression_summary(if_expression),
            Expression::Array(elements) => {
                let summaries: Vec<Summary> = elements.iter().map(|e| self.summarize(e)).collect();
                self.check_siblings(&summaries);
                merged(&summaries)
            }
            Expression::Binary { lhs, rhs, .. } => {
                let summaries = vec![self.summarize(lhs), self.summarize(rhs)];
                self.check_siblings(&summaries);
                merged(&summaries)
            }
        }
    }

    fn call_summary(&mut self, call: &'a FunctionCall) -> Summary {
        let summaries: Vec<Summary> = call
            .arguments
            .iter()
            .map(|argument| self.summarize(argument))
            .collect();
        self.check_siblings(&summaries);
        let mut summary = merged(&summaries);
        summary.call = true;
        let stateful = resolve_call(self.ctx, &call.function).is_some_and(|c| c.is_stateful());
        if stateful {
            summary.stateful = Some(lexeme(&call.function));
            if self.stateless {
                self.diags.push(GalecError::StatelessCallsStateful {
                    location: self.cursor.here(),
                    callee: lexeme(&call.function),
                });
            }
        }
        summary
    }

    /// If-expressions must not contain stateful calls (trap T12); reporting
    /// here also covers the sibling rule for their subtrees.
    fn if_expression_summary(&mut self, if_expression: &'a crate::ast::IfExpression) -> Summary {
        let mut summaries = Vec::new();
        for (condition, value) in &if_expression.branches {
            summaries.push(self.summarize(condition));
            summaries.push(self.summarize(value));
        }
        summaries.push(self.summarize(&if_expression.else_value));
        let summary = merged(&summaries);
        if summary.stateful.is_some() {
            self.report_stateful_calls_in(if_expression);
        }
        summary
    }

    fn report_stateful_calls_in(&mut self, if_expression: &'a crate::ast::IfExpression) {
        let ctx = self.ctx;
        let mut expressions: Vec<&'a Expression> = Vec::new();
        for (condition, value) in &if_expression.branches {
            expressions.push(condition);
            expressions.push(value);
        }
        expressions.push(&if_expression.else_value);
        for expression in expressions {
            let mut callees: Vec<String> = Vec::new();
            for_each_stateful_call(ctx, expression, &mut |call| {
                callees.push(lexeme(&call.function));
            });
            for callee in callees {
                self.diags.push(GalecError::StatefulCallInIfExpression {
                    location: self.cursor.here(),
                    callee,
                });
            }
        }
    }

    /// A subtree containing a stateful call must have no sibling subtree
    /// containing a call or a state-reference (§3.2.4 stateful-call
    /// isolation; unnumbered `S-TODO` in Beta-1).
    fn check_siblings(&mut self, summaries: &[Summary]) {
        for (index, summary) in summaries.iter().enumerate() {
            let Some(callee) = &summary.stateful else {
                continue;
            };
            let sibling_effects = summaries
                .iter()
                .enumerate()
                .any(|(other, s)| other != index && (s.call || s.state_ref));
            if sibling_effects {
                self.diags.push(GalecError::StatefulCallNotIsolated {
                    location: self.cursor.here(),
                    callee: callee.clone(),
                });
                return;
            }
        }
    }
}

fn merged(summaries: &[Summary]) -> Summary {
    summaries
        .iter()
        .fold(Summary::default(), |acc, s| acc.merge(s.clone()))
}

fn for_each_stateful_call<'a>(
    ctx: &BlockContext<'a>,
    expression: &'a Expression,
    visit: &mut impl FnMut(&'a FunctionCall),
) {
    match expression {
        Expression::Bool(_)
        | Expression::Integer(_)
        | Expression::Real(_)
        | Expression::Ref(_)
        | Expression::Neg(_)
        | Expression::Size { .. } => {}
        Expression::Call(call) => {
            if resolve_call(ctx, &call.function).is_some_and(|c| c.is_stateful()) {
                visit(call);
            }
            for argument in &call.arguments {
                for_each_stateful_call(ctx, argument, visit);
            }
        }
        Expression::Paren(inner) | Expression::Not(inner) => {
            for_each_stateful_call(ctx, inner, visit);
        }
        // A nested if-expression is a reporting boundary: it is independently
        // summarized (every if-expression flows through `if_expression_summary`
        // via `summarize`), so it reports its OWN stateful calls. Descending
        // here would re-report them once per enclosing if-expression level —
        // violating "each defect diagnosed exactly once".
        Expression::If(_) => {}
        Expression::Array(elements) => {
            for element in elements {
                for_each_stateful_call(ctx, element, visit);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            for_each_stateful_call(ctx, lhs, visit);
            for_each_stateful_call(ctx, rhs, visit);
        }
    }
}
