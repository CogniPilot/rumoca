//! Expression evaluation.
//!
//! Every operator is one match arm and every arm is total. There is no
//! constant folding, no caching, and no short-circuit cleverness: `if` picks a
//! branch, and everything else evaluates both operands. A reader checking this
//! against MLS §3 should be able to do it by reading top to bottom.

use std::collections::BTreeMap;

use crate::model::{BinaryOp, Expr};
use crate::value::Value;

/// The variable bindings an expression is evaluated against.
///
/// `pre(v)` always reads [`Environment::committed`]. There is no second mode
/// for the initialization instant, and getting that right took a correction
/// worth recording:
///
/// MLS §8.6 says "before the start of the integration, it must be guaranteed
/// that for all variables `v`, `v = pre(v)`", which reads like an instruction
/// to make `pre` the identity during initialization. Implemented that way,
/// `edge(b)` becomes `b and not b` — false for every buffer — and
/// `when initial() then ...` can never fire, contradicting §8.6's own rule that
/// such a clause *is* active during initialization.
///
/// The reading that satisfies both is that §8.6 constrains the values `pre`
/// *starts from*, not the meaning of the operator. So `committed` is seeded
/// from the start-value environment, which makes `pre(v) == v` hold on entry to
/// the initial instant for every declared variable, while an activation buffer
/// gated on `initial()` still sees a rising edge there — its seed is the
/// condition evaluated with `initial()` false, because `initial()` is true at
/// that one instant and false everywhere else.
#[derive(Debug, Clone)]
pub struct Environment {
    /// Current values.
    pub current: BTreeMap<String, Value>,
    /// Values `pre` reads: seeded from start values, then committed per event.
    pub committed: BTreeMap<String, Value>,
    /// The value of `initial()`.
    pub initial: bool,
    /// The value of `time`.
    pub time: f64,
}

/// Why an evaluation could not produce a value.
///
/// The reference refuses rather than guesses. A model outside slice 1 must be
/// visibly outside it, because a reference that silently coerced would let the
/// pipeline agree with it for the wrong reason.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// A name the environment does not bind.
    UnboundVariable(String),
    /// An operator applied to a type it is not defined on.
    TypeMismatch {
        /// What was being evaluated.
        context: String,
    },
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnboundVariable(name) => write!(f, "unbound variable `{name}`"),
            EvalError::TypeMismatch { context } => write!(f, "type mismatch in {context}"),
        }
    }
}

impl std::error::Error for EvalError {}

impl Environment {
    /// The current value bound to `name`.
    fn lookup(&self, name: &str) -> Result<Value, EvalError> {
        self.current
            .get(name)
            .copied()
            .ok_or_else(|| EvalError::UnboundVariable(name.to_owned()))
    }

    /// `pre(name)`: the value committed before the current instant.
    fn lookup_pre(&self, name: &str) -> Result<Value, EvalError> {
        self.committed
            .get(name)
            .copied()
            .ok_or_else(|| EvalError::UnboundVariable(name.to_owned()))
    }
}

/// Evaluates `expr` in `env`.
pub fn eval(expr: &Expr, env: &Environment) -> Result<Value, EvalError> {
    match expr {
        Expr::Literal(value) => Ok(*value),
        Expr::Variable(name) => env.lookup(name),
        Expr::Pre(name) => env.lookup_pre(name),
        // MLS §3.7.5: `edge(b)` is `b and not pre(b)`. Written out rather than
        // looked up, because this identity is the one the whole activation
        // semantics rests on.
        Expr::Edge(name) => {
            let current = truth(env.lookup(name)?, "edge")?;
            let previous = truth(env.lookup_pre(name)?, "edge")?;
            Ok(Value::Boolean(current && !previous))
        }
        Expr::Initial => Ok(Value::Boolean(env.initial)),
        Expr::Time => Ok(Value::Real(env.time)),
        Expr::Not(operand) => Ok(Value::Boolean(!truth(eval(operand, env)?, "not")?)),
        Expr::Negate(operand) => negate(eval(operand, env)?),
        Expr::Binary(op, lhs, rhs) => binary(*op, eval(lhs, env)?, eval(rhs, env)?),
        Expr::If(condition, consequent, alternative) => {
            if truth(eval(condition, env)?, "if")? {
                eval(consequent, env)
            } else {
                eval(alternative, env)
            }
        }
    }
}

/// A value's truth, or a type error naming the operator that wanted one.
fn truth(value: Value, context: &str) -> Result<bool, EvalError> {
    value.as_bool().ok_or_else(|| EvalError::TypeMismatch {
        context: context.to_owned(),
    })
}

/// `-value`.
fn negate(value: Value) -> Result<Value, EvalError> {
    match value {
        Value::Real(inner) => Ok(Value::Real(-inner)),
        Value::Integer(inner) => Ok(Value::Integer(-inner)),
        Value::Boolean(_) => Err(EvalError::TypeMismatch {
            context: "unary minus".to_owned(),
        }),
    }
}

/// Applies a binary operator to two already-evaluated operands.
fn binary(op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, EvalError> {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => arithmetic(op, lhs, rhs),
        BinaryOp::Less
        | BinaryOp::LessEqual
        | BinaryOp::Greater
        | BinaryOp::GreaterEqual
        | BinaryOp::Equal
        | BinaryOp::NotEqual => compare(op, lhs, rhs),
        BinaryOp::And | BinaryOp::Or => logic(op, lhs, rhs),
    }
}

/// `+ - * /`, staying in `Integer` when both operands are `Integer`.
///
/// MLS §10.6.13 widens `Integer` to `Real` in a mixed expression; `/` on two
/// `Integer` operands yields `Real` per MLS §3.4.
fn arithmetic(op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, EvalError> {
    if let (Value::Integer(left), Value::Integer(right)) = (lhs, rhs)
        && op != BinaryOp::Div
    {
        return integer_arithmetic(op, left, right);
    }
    let left = real_operand(lhs, "arithmetic")?;
    let right = real_operand(rhs, "arithmetic")?;
    Ok(Value::Real(match op {
        BinaryOp::Add => left + right,
        BinaryOp::Sub => left - right,
        BinaryOp::Mul => left * right,
        // Division by zero yields an IEEE infinity rather than an error. MLS
        // §3.7.1 leaves the result to the numeric model, and the host is where
        // a run decides whether that is fatal.
        _ => left / right,
    }))
}

/// `+ - *` on two `Integer` operands, saturating rather than wrapping.
///
/// MLS does not define `Integer` overflow. Saturating is the choice that cannot
/// silently produce a plausible wrong answer; a model that reaches the bound is
/// outside anything slice 1 claims to model.
fn integer_arithmetic(op: BinaryOp, left: i64, right: i64) -> Result<Value, EvalError> {
    Ok(Value::Integer(match op {
        BinaryOp::Add => left.saturating_add(right),
        BinaryOp::Sub => left.saturating_sub(right),
        _ => left.saturating_mul(right),
    }))
}

/// A numeric operand as `f64`, or a type error.
fn real_operand(value: Value, context: &str) -> Result<f64, EvalError> {
    value.as_real().ok_or_else(|| EvalError::TypeMismatch {
        context: context.to_owned(),
    })
}

/// Relational and equality operators.
fn compare(op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, EvalError> {
    if let (Value::Boolean(left), Value::Boolean(right)) = (lhs, rhs) {
        return match op {
            BinaryOp::Equal => Ok(Value::Boolean(left == right)),
            BinaryOp::NotEqual => Ok(Value::Boolean(left != right)),
            _ => Err(EvalError::TypeMismatch {
                context: "relational operator on Boolean".to_owned(),
            }),
        };
    }
    let left = real_operand(lhs, "comparison")?;
    let right = real_operand(rhs, "comparison")?;
    Ok(Value::Boolean(match op {
        BinaryOp::Less => left < right,
        BinaryOp::LessEqual => left <= right,
        BinaryOp::Greater => left > right,
        BinaryOp::GreaterEqual => left >= right,
        BinaryOp::Equal => left == right,
        _ => left != right,
    }))
}

/// `and` / `or`, both operands evaluated.
fn logic(op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, EvalError> {
    let left = truth(lhs, "logical operator")?;
    let right = truth(rhs, "logical operator")?;
    Ok(Value::Boolean(match op {
        BinaryOp::And => left && right,
        _ => left || right,
    }))
}
