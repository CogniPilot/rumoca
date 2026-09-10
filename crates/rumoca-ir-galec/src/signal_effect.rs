//! How a GALEC construct touches the block's `ErrorSignalStatus` word, and
//! the branded permission an optimization needs before it may change how many
//! times a construct is evaluated (SPEC_0034 GAL-040).
//!
//! # The contract this module enforces
//!
//! `ErrorSignalStatus` is cleared exactly once on entry to each of the three
//! block methods, and every other emitted write is a bitwise OR of a
//! compile-time-constant mask. `|=` with a constant mask is idempotent and
//! commutative, so inside one method the final word depends on the SET of
//! raising evaluations that ran, never on their count or their order.
//!
//! That makes evaluation *count* unobservable and licenses hoisting a repeated
//! evaluation down to one. It is not free of conditions:
//!
//! - a raise must still happen on exactly the executions where the original
//!   form raised at least once, so an optimization may never make a raise
//!   reachable that the original never reached; and
//! - a GALEC signal check (`if signal …`) *catches*: it clears the bits it
//!   tests (SPEC_0042 trap T10) and branches on them, which is both a read and
//!   a non-monotone write. The monotone argument therefore holds only between a
//!   method's reset and its first signal check.
//!
//! # Why a classification and a token rather than a predicate
//!
//! Per `dev/2026-08-26-compiler-architecture-decisions.md` D1 the optimizer is
//! untrusted and only a small checker is verified, and per D2 a precondition
//! must be a token that only a proof can construct. [`StatusEffect`] is that
//! checker: the classifiers below are exhaustive matches over the AST, so a
//! construct added later cannot be silently treated as safe: the author must
//! write an arm and choose. [`RepeatableSignalEffect`] is that token: its field
//! is private to this module and [`RepeatableSignalEffect::prove`] is the only
//! way to obtain one.
//!
//! The classification is a claim about *emitted C*, which no Rust type can
//! constrain. `crates/rumoca-phase-codegen/tests/galec_c_error_signal.rs` is the
//! other half: it renders the C error-signal surface and asserts the three
//! resets, the `|=`-with-constant shape of every other write, and the absence of
//! any read. A kernel added with a non-idempotent write fails that test.

use crate::ast::{Condition, Expression, FunctionCall, PrecedenceClass, Statement};
use crate::builtins::BUILTINS;

/// How one construct touches the block's `ErrorSignalStatus` word.
///
/// The variants form a lattice ordered `Inert < AccumulateOr < Consume <
/// Opaque`, joined by [`StatusEffect::join`]: a compound construct's effect is
/// the join of its parts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum StatusEffect {
    /// Cannot reach the word at all.
    Inert,
    /// Only ever ORs a compile-time-constant mask into the word. Idempotent and
    /// order-independent; nothing is read back.
    AccumulateOr,
    /// Catches signals: clears the bits it tests and branches on them. Both a
    /// read and a non-monotone write.
    Consume,
    /// Reaches the word in a way this analysis cannot bound, such as a call to a body
    /// this module cannot see.
    Opaque,
}

impl StatusEffect {
    /// The weaker of two guarantees, which is the effect of doing both.
    #[must_use]
    pub fn join(self, other: Self) -> Self {
        if self >= other { self } else { other }
    }

    /// Whether repeating this construct inside one signal-check-free region
    /// leaves the final `ErrorSignalStatus` word bit-identical.
    ///
    /// `Consume` is excluded because catching is not monotone, and `Opaque`
    /// because an unseen body may do either.
    #[must_use]
    pub fn is_repeatable(self) -> bool {
        matches!(self, Self::Inert | Self::AccumulateOr)
    }
}

/// Evidence that re-evaluating one expression a different number of times
/// inside one signal-check-free region leaves the final `ErrorSignalStatus`
/// word bit-identical (SPEC_0034 GAL-040).
///
/// The unit field is private to this module, so the only way to hold one is
/// [`RepeatableSignalEffect::prove`]. It is deliberately NOT `Default`,
/// `Deserialize` or constructible from a [`StatusEffect`] a caller chose: a
/// permission that a later edit could spell out by hand is a boolean predicate
/// wearing a type's clothes.
///
/// The token says nothing about loop invariance, trip counts, or where the
/// evaluation may move to. Those are separate obligations the holder must
/// discharge itself; this one covers the signal channel alone.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RepeatableSignalEffect(());

impl RepeatableSignalEffect {
    /// Mint the permission for `expression`, or decline.
    ///
    /// Declining is free (D2): a missed optimization is invisible, a wrong one
    /// changes an eFMI-visible output.
    #[must_use]
    pub fn prove(expression: &Expression) -> Option<Self> {
        expression_effect(expression)
            .is_repeatable()
            .then_some(Self(()))
    }

    /// Mint the permission for a branch condition, or decline.
    ///
    /// A signal check always declines: it classifies [`StatusEffect::Consume`].
    #[must_use]
    pub fn prove_condition(condition: &Condition) -> Option<Self> {
        condition_effect(condition)
            .is_repeatable()
            .then_some(Self(()))
    }
}

/// The effect of evaluating `expression`.
///
/// Real relational and equality operators raise `NAN` on a qNaN operand
/// (SPEC_0042 trap T9) and are therefore `AccumulateOr`, not `Inert`. The
/// operand types are not consulted: an Integer comparison raises nothing, so
/// calling it `AccumulateOr` is conservative in the safe direction and keeps
/// this classifier free of the type table.
#[must_use]
pub fn expression_effect(expression: &Expression) -> StatusEffect {
    match expression {
        // Literals, references, unary minus over a reference, and `size` are
        // all static or pure. `size` is applied at Production Code generation
        // time, where signaling is not permitted at all.
        Expression::Bool(_)
        | Expression::Integer(_)
        | Expression::Real(_)
        | Expression::Ref(_)
        | Expression::Neg(_)
        | Expression::Size { .. } => StatusEffect::Inert,
        Expression::Paren(inner) | Expression::Not(inner) => expression_effect(inner),
        Expression::Call(call) => call_effect(call),
        Expression::Array(elements) => join_all(elements.iter().map(expression_effect)),
        Expression::If(value) => {
            let branches = value.branches.iter().flat_map(|(condition, result)| {
                [expression_effect(condition), expression_effect(result)]
            });
            join_all(branches).join(expression_effect(&value.else_value))
        }
        Expression::Binary { op, lhs, rhs } => {
            let operands = expression_effect(lhs).join(expression_effect(rhs));
            if matches!(
                op.precedence_class(),
                PrecedenceClass::Relational | PrecedenceClass::Equality
            ) {
                operands.join(StatusEffect::AccumulateOr)
            } else {
                operands
            }
        }
    }
}

/// The effect of evaluating a branch condition.
#[must_use]
pub fn condition_effect(condition: &Condition) -> StatusEffect {
    match condition {
        Condition::Expression(expression) => expression_effect(expression),
        // Checking is catching (trap T10): the check clears the bits it tests.
        // Its `or expr` fallback is joined so that a caller reading only this
        // value still sees everything the condition can do, even though
        // `Consume` already dominates.
        Condition::SignalCheck(check) => check
            .fallback
            .as_ref()
            .map_or(StatusEffect::Consume, |fallback| {
                expression_effect(fallback).join(StatusEffect::Consume)
            }),
    }
}

/// The effect of executing `statement`.
#[must_use]
pub fn statement_effect(statement: &Statement) -> StatusEffect {
    match statement {
        Statement::Assignment { target: _, value } => expression_effect(value),
        Statement::MultiAssignment { targets: _, call } | Statement::Call(call) => {
            call_effect(call)
        }
        // `signal s1, …;` is emitted as `status |= <constant>`, one constant
        // per named signal; a closure name re-raises a set fixed at the check
        // that captured it. Either way the mask does not depend on the word.
        Statement::Signal(_) => StatusEffect::AccumulateOr,
        Statement::If(value) => {
            let branches = value.branches.iter().flat_map(|branch| {
                [condition_effect(&branch.condition)]
                    .into_iter()
                    .chain(branch.body.iter().map(|s| statement_effect(&s.node)))
            });
            let else_body = value
                .else_body
                .iter()
                .flatten()
                .map(|s| statement_effect(&s.node));
            join_all(branches.chain(else_body))
        }
        Statement::For(value) => {
            let bounds = [
                expression_effect(&value.start),
                expression_effect(&value.stop),
            ];
            let step = value.step.as_ref().map(expression_effect);
            let body = value.body.iter().map(|s| statement_effect(&s.node));
            join_all(bounds.into_iter().chain(step).chain(body))
        }
        // Declared-range saturation raises no signal and reads nothing
        // (SPEC_0042 T3).
        Statement::Limit(_) => StatusEffect::Inert,
    }
}

/// The effect of one call.
///
/// A builtin's signal set is fixed by the §3.2.6 catalog and every emitted
/// raise of it is an OR of that constant mask, so a builtin is `AccumulateOr`
/// at worst. A user function is `Opaque`: its body is not in scope here, and
/// resolving it would make this classifier depend on a symbol table it must not
/// need. Callers that can resolve the callee may classify its body themselves
/// and join the result.
fn call_effect(call: &FunctionCall) -> StatusEffect {
    let arguments = join_all(call.arguments.iter().map(expression_effect));
    let name = call.function.lexeme();
    let builtin = BUILTINS.iter().any(|entry| entry.name == name);
    if builtin {
        arguments.join(StatusEffect::AccumulateOr)
    } else {
        arguments.join(StatusEffect::Opaque)
    }
}

fn join_all(effects: impl IntoIterator<Item = StatusEffect>) -> StatusEffect {
    effects
        .into_iter()
        .fold(StatusEffect::Inert, StatusEffect::join)
}

#[cfg(test)]
mod tests;
