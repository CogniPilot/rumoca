//! The predefined scalar type a declaration gives its values, and the
//! conversion a value takes when it is stored in that declaration.
//!
//! MLS 3.7 §10.6.13 lets an Integer stand where a Real is expected and
//! converts it; nothing converts the other way. A value bound to a function
//! input, assigned to an output, local or record field, or settled as a
//! parameter's value is converted here, once, so that later arithmetic on it
//! is the declared type's arithmetic (a negated Real zero is `-0.0`, a product
//! beyond the Integer range is a finite Real). The declared type is read from
//! the declaration's canonical type identity, never from its spelling.

use rumoca_core::{EffectiveType, Span};
use rumoca_ir_flat::PredefinedTypeIds;

use super::errors::EvalError;
use super::value::Value;

/// A predefined scalar type of a declaration (MLS 3.7 §4.9).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeclaredScalar {
    Real,
    Integer,
    Boolean,
    String,
}

impl DeclaredScalar {
    /// The predefined scalar type `effective_type` resolves to; `None` for
    /// an enumeration, a record or any other non-predefined type.
    pub fn of(effective_type: &EffectiveType, predefined: &PredefinedTypeIds) -> Option<Self> {
        let canonical = effective_type.canonical_type();
        [
            (predefined.real, Self::Real),
            (predefined.integer, Self::Integer),
            (predefined.boolean, Self::Boolean),
            (predefined.string, Self::String),
        ]
        .into_iter()
        .find_map(|(id, declared)| (id == canonical).then_some(declared))
    }

    /// `value` stored in a declaration of this type: every Integer cell of a
    /// Real declaration becomes Real; a cell of any other type than the
    /// declared one is refused.
    pub fn convert(self, value: Value, span: Span) -> Result<Value, EvalError> {
        match (self, value) {
            (_, Value::Array(cells)) => cells
                .into_iter()
                .map(|cell| self.convert(cell, span))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Array),
            (Self::Real, Value::Integer(value)) => Ok(Value::Real(value as f64)),
            (Self::Real, value @ Value::Real(_))
            | (Self::Integer, value @ Value::Integer(_))
            | (Self::Boolean, value @ Value::Bool(_))
            | (Self::String, value @ Value::String(_)) => Ok(value),
            (declared, value) => Err(EvalError::type_mismatch(
                declared.name(),
                value.type_name(),
                span,
            )),
        }
    }

    fn name(self) -> &'static str {
        match self {
            Self::Real => "Real",
            Self::Integer => "Integer",
            Self::Boolean => "Boolean",
            Self::String => "String",
        }
    }
}
