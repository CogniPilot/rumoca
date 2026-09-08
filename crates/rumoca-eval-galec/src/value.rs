use std::collections::BTreeMap;

/// Runtime value of a checked GALEC declaration.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    Integer(i64),
    Real(f64),
    Array(Vec<Value>),
    Record(BTreeMap<String, Value>),
    /// Shape-preserving storage which has not received a semantic value yet.
    #[doc(hidden)]
    Uninitialized,
}

impl Value {
    pub(crate) fn boolean(&self) -> Option<bool> {
        match self {
            Self::Boolean(value) => Some(*value),
            _ => None,
        }
    }

    pub(crate) fn integer(&self) -> Option<i64> {
        match self {
            Self::Integer(value) => Some(*value),
            _ => None,
        }
    }

    pub(crate) fn real(&self) -> Option<f64> {
        match self {
            Self::Real(value) => Some(*value),
            _ => None,
        }
    }

    pub(crate) fn is_initialized(&self) -> bool {
        match self {
            Self::Array(values) => values.iter().all(Self::is_initialized),
            Self::Record(fields) => fields.values().all(Self::is_initialized),
            Self::Boolean(_) | Self::Integer(_) | Self::Real(_) => true,
            Self::Uninitialized => false,
        }
    }
}
