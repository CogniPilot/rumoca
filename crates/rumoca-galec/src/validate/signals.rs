//! Signal analysis (§3.2.5 §1.5 static signal propagation, GAL-018, trap
//! T10): compute each body's escape set (the out-reachable-signals-set an
//! imaginary statement after its last statement would have) and require the
//! declared `signals` clause to EXACTLY equal it — both directions are
//! errors. Signal checks catch (unset their test-set); testing an
//! unsettable/already-caught signal and empty test-sets are errors; at most
//! 16 user signals fit the 32-bit encoding; block-interface methods expose
//! only predefined signals (guaranteed structurally by
//! [`crate::ast::BlockMethod::signals`] being `Vec<PredefinedSignal>`).
//!
//! Dataflow reading choices (documented deviations-free interpretations of
//! Beta-1 §1.5):
//!
//! - "union of out-sets of all preceding statements (control-flow)" is read
//!   as the control-flow-graph predecessor: in a statement list each
//!   statement has exactly one predecessor, and if/for out-sets already
//!   union their internal paths. This makes the normative E-3/E-4
//!   consequences (an immediately repeated identical check is illegal)
//!   hold.
//! - A signal-closure statically captures the full in-reachable set of its
//!   check condition (the closure is initialized before the test-set is
//!   unset), so `signal s;` re-raises at most that set.
//! - For-loops are analyzed single-pass per the normative rule (the loop's
//!   signal-set is the out-set of its last statement seeded with the loop's
//!   in-set); the statement out-set unions the loop's in-set, so a zero-trip
//!   loop never loses signals.
//!
//! Slice-2 hook (SPEC_0034 D8, trap T9): Real relational operators signal
//! `NAN` when an operand is qNaN. Slice 1 accounts no relational signals;
//! see [`SignalWalker::relational_signal_set`] — enabling NAN accounting is
//! a local change there (adding operand-type awareness), with no dataflow
//! restructuring.

use crate::ast::{
    BinaryOp, Condition, Expression, FunctionCall, Identifier, IfStatement, SignalCheck, Statement,
};
use crate::diagnostic::{GalecError, PathSegment};

use super::context::{BlockContext, BodyView, Cursor, SignalSet, SignalTable, resolve_call};

/// User-defined signal budget in the 32-bit encoding (§3.2.5 §1.6).
const MAX_USER_SIGNALS: usize = 16;

pub(super) fn check(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    if ctx.signals.user_count() > MAX_USER_SIGNALS {
        diags.push(GalecError::TooManyUserSignals {
            location: Cursor::for_block(ctx).here(),
            count: ctx.signals.user_count(),
        });
    }
    for body in ctx.bodies() {
        check_body(ctx, &body, diags);
    }
}

fn check_body(ctx: &BlockContext<'_>, body: &BodyView<'_>, diags: &mut Vec<GalecError>) {
    let mut walker = SignalWalker {
        ctx,
        cursor: Cursor::for_body(ctx, body),
        closures: Vec::new(),
        diags,
    };
    let declared = walker.declared_set(body);
    let computed = walker.statements(body.statements, SignalSet::default());
    let location = Cursor::for_body(ctx, body).here();
    for bit in computed.difference(&declared).iter() {
        walker.diags.push(GalecError::UndeclaredEscape {
            location: location.clone(),
            function: body.name.clone(),
            signal: ctx.signals.name(bit).to_string(),
        });
    }
    for bit in declared.difference(&computed).iter() {
        walker.diags.push(GalecError::OverdeclaredSignal {
            location: location.clone(),
            function: body.name.clone(),
            signal: ctx.signals.name(bit).to_string(),
        });
    }
}

struct SignalWalker<'a, 'd> {
    ctx: &'a BlockContext<'a>,
    cursor: Cursor,
    /// Signal-closures in scope: (name, statically captured set), LIFO.
    closures: Vec<(String, SignalSet)>,
    diags: &'d mut Vec<GalecError>,
}

impl<'a> SignalWalker<'a, '_> {
    /// The declared signal clause as a set. Methods expose predefined
    /// signals by construction; user clauses must name declared signals.
    fn declared_set(&mut self, body: &BodyView<'_>) -> SignalSet {
        let mut declared = SignalSet::default();
        if let Some(method) = body.method {
            let signals = match method {
                crate::ast::BlockMethodKind::Startup => &self.ctx.block.startup.signals,
                crate::ast::BlockMethodKind::Recalibrate => &self.ctx.block.recalibrate.signals,
                crate::ast::BlockMethodKind::DoStep => &self.ctx.block.do_step.signals,
            };
            for signal in signals {
                declared.insert(SignalTable::predefined_bit(*signal));
            }
        } else if let Some(function) = body.user {
            for id in &function.signals {
                match self.ctx.signals.bit(&id.0) {
                    Some(bit) => declared.insert(bit),
                    None => self.unknown_signal(id),
                }
            }
        }
        declared
    }

    // -----------------------------------------------------------------
    // Dataflow over statements
    // -----------------------------------------------------------------

    fn statements(&mut self, statements: &[Statement], mut set: SignalSet) -> SignalSet {
        for (index, statement) in statements.iter().enumerate() {
            self.cursor.push(PathSegment::Statement(index));
            set = self.statement(statement, set);
            self.cursor.pop();
        }
        set
    }

    fn statement(&mut self, statement: &Statement, in_set: SignalSet) -> SignalSet {
        match statement {
            Statement::Assignment { value, .. } => {
                let mut out = in_set;
                out.union_with(&self.expression_signals(value));
                out
            }
            Statement::MultiAssignment { call, .. } | Statement::Call(call) => {
                let mut out = in_set;
                out.union_with(&self.call_signals(call));
                out
            }
            Statement::Signal(identifiers) => self.signal_statement(identifiers, in_set),
            Statement::If(if_statement) => self.if_statement(if_statement, in_set),
            Statement::For(for_loop) => {
                // Bounds are static (builtin-only, non-signaling by S-2.9).
                let body_out = self.statements(&for_loop.body, in_set.clone());
                let mut out = in_set;
                out.union_with(&body_out);
                out
            }
            Statement::Limit(_) => in_set,
        }
    }

    /// `signal a, s, …;` sets each named signal; a closure name re-raises
    /// its captured set (§1.2).
    fn signal_statement(&mut self, identifiers: &[Identifier], in_set: SignalSet) -> SignalSet {
        let mut out = in_set;
        for id in identifiers {
            if let Some(captured) = self.closure_set(&id.0) {
                out.union_with(&captured);
            } else if let Some(bit) = self.ctx.signals.bit(&id.0) {
                out.insert(bit);
            } else {
                self.unknown_signal(id);
            }
        }
        out
    }

    fn if_statement(&mut self, if_statement: &IfStatement, in_set: SignalSet) -> SignalSet {
        let mut condition_in = in_set;
        let mut body_union = SignalSet::default();
        for (index, branch) in if_statement.branches.iter().enumerate() {
            self.cursor.push(PathSegment::Branch(index));
            let (condition_out, closure) = self.condition(&branch.condition, &condition_in);
            let pushed = closure.is_some();
            if let Some(scoped) = closure {
                self.closures.push(scoped);
            }
            let body_out = self.statements(&branch.body, condition_out.clone());
            if pushed {
                self.closures.pop();
            }
            body_union.union_with(&body_out);
            condition_in = condition_out;
            self.cursor.pop();
        }
        if let Some(else_body) = &if_statement.else_body {
            self.cursor.push(PathSegment::Else);
            let else_out = self.statements(else_body, condition_in.clone());
            body_union.union_with(&else_out);
            self.cursor.pop();
        }
        let mut out = condition_in;
        out.union_with(&body_union);
        out
    }

    /// Out-set of a branch condition (§1.5); for signal checks this also
    /// validates the test-set (non-empty, settable) and yields the closure
    /// capture. A branch body's in-set equals its condition's out-set.
    fn condition(
        &mut self,
        condition: &Condition,
        in_set: &SignalSet,
    ) -> (SignalSet, Option<(String, SignalSet)>) {
        match condition {
            Condition::Expression(expression) => {
                let mut out = in_set.clone();
                out.union_with(&self.expression_signals(expression));
                (out, None)
            }
            Condition::SignalCheck(check) => self.signal_check(check, in_set),
        }
    }

    fn signal_check(
        &mut self,
        check: &SignalCheck,
        in_set: &SignalSet,
    ) -> (SignalSet, Option<(String, SignalSet)>) {
        self.cursor.push(PathSegment::Condition);
        let test_set = self.test_set(check, in_set);
        let mut out = in_set.clone();
        out.remove_all(&test_set);
        if let Some(fallback) = &check.fallback {
            out.union_with(&self.expression_signals(fallback));
        }
        self.cursor.pop();
        let closure = check
            .closure
            .as_ref()
            .map(|id| (id.0.clone(), in_set.clone()));
        (out, closure)
    }

    /// The signal-test-set (§1.4): listed signals must be settable here;
    /// `not in` and unrestricted checks test the in-reachable remainder,
    /// which must be non-empty.
    fn test_set(&mut self, check: &SignalCheck, in_set: &SignalSet) -> SignalSet {
        match &check.test {
            Some(test) if !test.negated => self.listed_test_set(&test.signals, in_set),
            Some(test) => {
                let excluded = self.signal_ids(&test.signals);
                let set = in_set.difference(&excluded);
                if set.is_empty() {
                    self.empty_test_set();
                }
                set
            }
            None => {
                if in_set.is_empty() {
                    self.empty_test_set();
                }
                in_set.clone()
            }
        }
    }

    /// `in s1, s2, …`: every listed signal must be in the in-reachable set
    /// (testing an unsettable or already-caught signal is illegal, T10).
    fn listed_test_set(&mut self, ids: &[Identifier], in_set: &SignalSet) -> SignalSet {
        let mut set = SignalSet::default();
        for id in ids {
            let Some(bit) = self.ctx.signals.bit(&id.0) else {
                self.unknown_signal(id);
                continue;
            };
            if in_set.contains(bit) {
                set.insert(bit);
                continue;
            }
            self.diags.push(GalecError::UntestableSignal {
                location: self.cursor.here(),
                signal: id.0.clone(),
            });
        }
        set
    }

    /// Resolve a list of signal identifiers to bits, reporting unknowns.
    fn signal_ids(&mut self, ids: &[Identifier]) -> SignalSet {
        let mut set = SignalSet::default();
        for id in ids {
            match self.ctx.signals.bit(&id.0) {
                Some(bit) => set.insert(bit),
                None => self.unknown_signal(id),
            }
        }
        set
    }

    // -----------------------------------------------------------------
    // Signal-sets of expressions and calls (§1.5)
    // -----------------------------------------------------------------

    fn expression_signals(&mut self, expression: &Expression) -> SignalSet {
        match expression {
            Expression::Bool(_)
            | Expression::Integer(_)
            | Expression::Real(_)
            | Expression::Ref(_)
            | Expression::Neg(_)
            // size() and subscripts are static: applied at Production Code
            // generation time, where signaling is not permitted (S-2.9).
            | Expression::Size { .. } => SignalSet::default(),
            Expression::Call(call) => self.call_signals(call),
            Expression::Paren(inner) | Expression::Not(inner) => self.expression_signals(inner),
            Expression::If(if_expression) => {
                let mut set = SignalSet::default();
                for (condition, value) in &if_expression.branches {
                    set.union_with(&self.expression_signals(condition));
                    set.union_with(&self.expression_signals(value));
                }
                set.union_with(&self.expression_signals(&if_expression.else_value));
                set
            }
            Expression::Array(elements) => {
                let mut set = SignalSet::default();
                for element in elements {
                    set.union_with(&self.expression_signals(element));
                }
                set
            }
            Expression::Binary { op, lhs, rhs } => {
                let mut set = self.expression_signals(lhs);
                set.union_with(&self.expression_signals(rhs));
                set.union_with(&Self::relational_signal_set(*op));
                set
            }
        }
    }

    /// The signal-set of a call is the callee's declared signal-set plus
    /// the argument expressions' sets. Only `integer()` and the three
    /// linear-solver builtins signal (trap T14); unknown callees contribute
    /// nothing (reported as EG015 by the type analysis).
    fn call_signals(&mut self, call: &FunctionCall) -> SignalSet {
        let mut set = match resolve_call(self.ctx, &call.function) {
            Some(callee) => callee.signal_set(&self.ctx.signals),
            None => SignalSet::default(),
        };
        for argument in &call.arguments {
            set.union_with(&self.expression_signals(argument));
        }
        set
    }

    /// Slice-2 hook (SPEC_0034 D8, trap T9): under full Beta-1 semantics a
    /// Real relational comparison signals `NAN` for qNaN operands. Slice 1
    /// accounts no relational signals; flipping this on later means
    /// returning `{NAN}` for relational/equality operators with Real
    /// operands (requires operand types, available from the type analysis).
    fn relational_signal_set(_op: BinaryOp) -> SignalSet {
        SignalSet::default()
    }

    // -----------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------

    fn closure_set(&self, name: &str) -> Option<SignalSet> {
        self.closures
            .iter()
            .rev()
            .find(|(closure, _)| closure == name)
            .map(|(_, set)| set.clone())
    }

    fn unknown_signal(&mut self, id: &Identifier) {
        self.diags.push(GalecError::UnknownSignal {
            location: self.cursor.here(),
            name: id.0.clone(),
        });
    }

    fn empty_test_set(&mut self) {
        self.diags.push(GalecError::EmptySignalTestSet {
            location: self.cursor.here(),
        });
    }
}
