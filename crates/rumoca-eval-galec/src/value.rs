use std::collections::BTreeMap;

/// Target-selected closed range for GALEC `Integer`.
///
/// GALEC deliberately makes this range target-specific. Callers must choose
/// it explicitly; the evaluator never assumes host `i64` semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntegerDomain {
    min: i64,
    max: i64,
}

impl IntegerDomain {
    pub const fn new(min: i64, max: i64) -> Option<Self> {
        if min <= 0 && max >= 0 && min < max {
            Some(Self { min, max })
        } else {
            None
        }
    }

    #[must_use]
    pub const fn signed_32() -> Self {
        Self {
            min: i32::MIN as i64,
            max: i32::MAX as i64,
        }
    }

    #[must_use]
    pub const fn min(self) -> i64 {
        self.min
    }

    #[must_use]
    pub const fn max(self) -> i64 {
        self.max
    }

    #[must_use]
    pub const fn contains(self, value: i64) -> bool {
        value >= self.min && value <= self.max
    }
}

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
