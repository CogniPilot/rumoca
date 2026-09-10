//! Values, environments and refusal reasons for the preservation interpreter.
//!
//! The value domain is deliberately narrow: scalar Real, Integer and Boolean
//! plus one-dimensional arrays of them. Every Real produced stays an exactly
//! representable integer-valued `f64` under [`Value::exact_real`], so two
//! programs that compute the same number by different associations compare
//! equal and a divergence reported by the differential is a real one.

use super::*;
use std::collections::BTreeMap;

/// Largest magnitude a Real value may reach and still be exact in `f64`.
const EXACT_REAL_LIMIT: f64 = (1u64 << 40) as f64;

/// A value the interpreter can hold.
#[derive(Clone, Debug, PartialEq)]
pub(super) enum Value {
    Real(f64),
    Integer(i64),
    Boolean(bool),
    Array(Vec<Value>),
}

impl Value {
    /// Wrap a computed Real, refusing anything outside the exact range.
    pub(super) fn exact_real(value: f64) -> Result<Self, Refusal> {
        if !value.is_finite() || value.fract() != 0.0 || value.abs() > EXACT_REAL_LIMIT {
            return Err(Refusal::InexactReal);
        }
        Ok(Value::Real(value))
    }

    pub(super) fn as_integer(&self) -> Result<i64, Refusal> {
        match self {
            Value::Integer(value) => Ok(*value),
            _ => Err(Refusal::TypeMismatch),
        }
    }

    pub(super) fn as_boolean(&self) -> Result<bool, Refusal> {
        match self {
            Value::Boolean(value) => Ok(*value),
            _ => Err(Refusal::TypeMismatch),
        }
    }

    /// A readable rendering used when a divergence names a value.
    pub(super) fn render(&self) -> String {
        match self {
            Value::Real(value) => format!("{value}"),
            Value::Integer(value) => format!("{value}"),
            Value::Boolean(value) => format!("{value}"),
            Value::Array(elements) => {
                let rendered = elements
                    .iter()
                    .map(Value::render)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{rendered}}}")
            }
        }
    }
}

/// A complete variable environment, keyed by flat name.
pub(super) type Environment = BTreeMap<VarName, Value>;

/// Why the interpreter declined to execute a program.
///
/// Every refusal is counted by reason in the differential ledger; none is
/// allowed to silently remove a program from the comparison.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum Refusal {
    /// A statement form outside the evaluable subset.
    UnsupportedStatement,
    /// An expression form outside the evaluable subset.
    UnsupportedExpression,
    /// An operator outside the evaluable subset.
    UnsupportedOperator,
    /// A read of a name the environment does not declare.
    UnknownName,
    /// An operand whose type does not admit the operation.
    TypeMismatch,
    /// A subscript outside the declared extent.
    SubscriptOutOfRange,
    /// A subscript list this interpreter does not model.
    UnsupportedSubscript,
    /// A `for` range that is not a finite ascending integer sequence.
    UnsupportedRange,
    /// A Real value outside the exactly representable range.
    InexactReal,
    /// Integer arithmetic that overflowed.
    IntegerOverflow,
    /// The statement budget ran out, which keeps the interpreter total.
    FuelExhausted,
}

impl Refusal {
    pub(super) const ALL: [Refusal; 11] = [
        Refusal::UnsupportedStatement,
        Refusal::UnsupportedExpression,
        Refusal::UnsupportedOperator,
        Refusal::UnknownName,
        Refusal::TypeMismatch,
        Refusal::SubscriptOutOfRange,
        Refusal::UnsupportedSubscript,
        Refusal::UnsupportedRange,
        Refusal::InexactReal,
        Refusal::IntegerOverflow,
        Refusal::FuelExhausted,
    ];

    pub(super) fn label(self) -> &'static str {
        match self {
            Refusal::UnsupportedStatement => "unsupported statement",
            Refusal::UnsupportedExpression => "unsupported expression",
            Refusal::UnsupportedOperator => "unsupported operator",
            Refusal::UnknownName => "unknown name",
            Refusal::TypeMismatch => "type mismatch",
            Refusal::SubscriptOutOfRange => "subscript out of range",
            Refusal::UnsupportedSubscript => "unsupported subscript",
            Refusal::UnsupportedRange => "unsupported range",
            Refusal::InexactReal => "inexact real",
            Refusal::IntegerOverflow => "integer overflow",
            Refusal::FuelExhausted => "fuel exhausted",
        }
    }
}

/// Render an environment restricted to `names`, sorted for stable reports.
pub(super) fn render_environment(environment: &Environment, names: &[VarName]) -> String {
    let mut rendered = names
        .iter()
        .map(|name| {
            let value = environment
                .get(name)
                .map(Value::render)
                .unwrap_or_else(|| "<absent>".to_string());
            format!("{}={value}", name.as_str())
        })
        .collect::<Vec<_>>();
    rendered.sort();
    rendered.join(" ")
}
