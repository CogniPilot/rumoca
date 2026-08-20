//! The three scalar types slice 1 admits, and their values.
//!
//! MLS §4.8 gives Modelica `Real`, `Integer`, `Boolean`, `String`, and
//! enumerations. Slice 1 takes the first three: they are the ones the event
//! core actually branches on, and leaving `String`/enumerations out keeps every
//! function in this crate small enough to read in one sitting.

/// A slice-1 scalar type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    /// MLS `Real`.
    Real,
    /// MLS `Integer`.
    Integer,
    /// MLS `Boolean`.
    Boolean,
}

/// A slice-1 scalar value.
///
/// `PartialEq` here is exact. The event iteration of MLS Appendix B terminates
/// on `z == pre(z)`, which is an exact comparison in the specification, and a
/// reference that used a tolerance would be answering a different question than
/// the one the specification asks.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    /// A `Real` value.
    Real(f64),
    /// An `Integer` value.
    Integer(i64),
    /// A `Boolean` value.
    Boolean(bool),
}

impl Value {
    /// The type this value inhabits.
    #[must_use]
    pub fn type_of(self) -> Type {
        match self {
            Value::Real(_) => Type::Real,
            Value::Integer(_) => Type::Integer,
            Value::Boolean(_) => Type::Boolean,
        }
    }

    /// The value's truth, or `None` when it is not a `Boolean`.
    ///
    /// Slice 1 does not coerce numbers to truth. MLS §3.5 admits no such
    /// conversion, and accepting one here would let the reference agree with a
    /// pipeline that had invented it.
    #[must_use]
    pub fn as_bool(self) -> Option<bool> {
        match self {
            Value::Boolean(value) => Some(value),
            _ => None,
        }
    }

    /// The value as a `Real`, widening an `Integer` per MLS §10.6.13.
    #[must_use]
    pub fn as_real(self) -> Option<f64> {
        match self {
            Value::Real(value) => Some(value),
            #[expect(
                clippy::cast_precision_loss,
                reason = "MLS §10.6.13 widens Integer to Real; the widening is the semantics being modelled, and slice 1 states no exactness claim beyond it"
            )]
            Value::Integer(value) => Some(value as f64),
            Value::Boolean(_) => None,
        }
    }
}
